;;; karuta-mode.el --- Major mode for the Karuta programming language -*- lexical-binding: t; -*-

;; Author: Marcos Magueta <maguetamarcos@gmail.com>
;; 	Estevan Castilho <tevo@riajuu.net>
;; URL: https://github.com/dont-rely-on-nulls/karuta
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `karuta-mode' is a major mode for editing Karuta source files (`.krt')
;; and Sakura database files (`.skr').  Karuta is a relational, aiming to
;; be constraint-based, programming language; the two file formats share
;; the same surface syntax but differ in semantics.

;;; Code:

(require 'karuta-faces)
(require 'cl-lib)

(defgroup karuta nil
  "Major mode for the Karuta programming language."
  :group 'languages
  :prefix "karuta-"
  :link '(url-link "https://github.com/dont-rely-on-nulls/karuta"))

;;;; Customization

(defcustom karuta-executable "karuta"
  "Path to the Karuta command-line toolchain."
  :type 'string
  :group 'karuta)

(defcustom karuta-compile-subcommand "compile"
  "Subcommand of `karuta-executable' used to compile a source file.
The default `compile' invocation is

    <karuta-executable> <karuta-compile-subcommand> <file>

which, out of the box, expands to `karuta compile source'."
  :type 'string
  :group 'karuta)

(defcustom karuta-indent-offset 2
  "Number of columns for each level of indentation."
  :type 'integer
  :safe #'integerp
  :group 'karuta)

;;;; Keywords recognized specially

(defconst karuta-keywords
  '("module" "signature" "project" "comment" "sakura"
    "persisted" "ephemeral" "constraint" "import" "karuta"
    "begin" "end")
  "Words that are highlighted with `karuta-keyword-face'.")

;;;; Syntax table

(defconst karuta-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Atoms may contain letters, digits, `_' and `-'; treat `_' and `-'
    ;; as symbol constituents so that `\\_<' / `\\_>' bound whole atoms.
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    ;; Brackets and braces balance.
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Comments
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?# "." table) ; see `karuta-syntax-propertize-function'
    ;; Punctuation
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    table)
  "Syntax table for `karuta-mode'.")

;;;; Expression scanning (shared by `#%' comments and, later, motion)

(defun karuta--skip-balanced (pos)
  "Return the position after the balanced bracket group starting at POS.
Fall back to end of line when the group is unbalanced."
  (or (ignore-errors (scan-sexps pos 1))
      (save-excursion (goto-char pos) (line-end-position))))

(defun karuta--expr-end (pos)
  "Return the end position of the Karuta expression starting at POS.
POS is expected to sit just after a `#%' marker; leading whitespace is
skipped.  Handles integers, variables, atoms/quoted-atoms optionally
applied to a balanced argument list, and bare lists.  Returns POS when no
expression is found."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\r\n")
    (let ((start (point)))
      (cond
       ;; integer
       ((looking-at "-?[0-9]+")
        (match-end 0))
       ;; variable
       ((looking-at "[A-Z_][a-zA-Z0-9_]*")
        (match-end 0))
       ;; quoted atom, optionally a functor
       ((looking-at "'\\(?:[^'\n]\\)*'")
        (goto-char (match-end 0))
        (if (memq (char-after) '(?\[ ?\())
            (karuta--skip-balanced (point))
          (point)))
       ;; atom, optionally qualified, optionally a functor
       ((looking-at "[a-z][a-zA-Z0-9_-]*\\(?:[:.][a-z][a-zA-Z0-9_-]*\\)*")
        (goto-char (match-end 0))
        (if (memq (char-after) '(?\[ ?\())
            (karuta--skip-balanced (point))
          (point)))
       ;; bare list
       ((eq (char-after) ?\[)
        (karuta--skip-balanced (point)))
       (t start)))))

(defun karuta--expression-end (start)
  (save-excursion
    (goto-char start)
    (forward-sexp)
    (when (looking-at "[[(]")
      (forward-sexp))
    (1- (point))))

(defun karuta--propertize-expression-comment ()
  (let ((start (match-beginning 0))
        (end (karuta--expression-end (match-end 0))))
    (put-text-property start (1+ start) 'syntax-table (string-to-syntax "!"))
    (put-text-property end (1+ end) 'syntax-table (string-to-syntax "!"))))

(defalias 'karuta-syntax-propertize-function
  (syntax-propertize-rules
   ;; Disallow strings spanning multiple lines
   ("\\(\"\\)\\(?:[^\"\n]*\\)\\(\n\\)" (1 "|") (2 "|"))
   ("\\(\"\\)\\(?:[^\"\n]*\\)\\(\"\\)" (1 "\"") (2 "\""))
   ;; Ditto for atoms
   ("\\('\\)\\(?:[^'\n]*\\)\\(\n\\)" (1 "|") (2 "|"))
   ("\\('\\)\\(?:[^'\n]*\\)\\('\\)" (1 "\"") (2 "\""))
   ;; Expression comments
   ("#%" (0 (karuta--propertize-expression-comment)))))

(defun karuta--quoted-atom-face (start)
  (save-excursion
    (goto-char start)
    (cond ((looking-at-p ":[a-z']") 'karuta-module-qualifier-face)
          ((looking-at-p "[[(]") 'karuta-predicate-face)
          (t 'karuta-atom-face))))

(defun karuta-syntactic-face-function (state)
  "Return the face for the syntactic construct described by STATE."
  (cl-destructuring-bind (_0 _1 _2 string? comment? _5 _6 _7 s-or-c-start &rest _
                             &aux (starts-with (when s-or-c-start (char-after s-or-c-start)))) state
    (declare (ignore _0 _1 _2 _5 _6 _7 _))
    (cond ((and string? (eql ?\' starts-with)) (karuta--quoted-atom-face s-or-c-start))
          (string? 'karuta-string-face)
          (comment? 'karuta-comment-face)
          (t nil))))

;;;; Font lock

(defun karuta--match-qualifier (limit)
  "Font-lock matcher for a module qualifier segment `atom:' before
LIMIT.  Matches the atom (group 1) and the separator (group 2) of a
qualified name such as `karuta:', `a:' in `a:b:pluz'.  Careful not to
match the arrow operator `:-'."
  (catch 'done
    (while (re-search-forward
            "\\_<\\([a-z][a-zA-Z0-9_-]*\\)\\(:\\)" limit t)
      (let ((sep (char-before))           ; the `:'
            (after (char-after)))         ; first char of the next segment
        ;; A real qualifier is followed by another atom or quoted atom;
        ;; this rules out the arrow `:-' and clause-terminating `.'.
        (when (and after
                   (or (and (>= after ?a) (<= after ?z))
                       (eq after ?\')))
          (ignore sep)
          (throw 'done t))))
    nil))

(defvar karuta-font-lock-keywords
  `(;; Special keywords (module, signature, import, karuta, ...).
    (,(concat "\\_<" (regexp-opt karuta-keywords t) "\\_>")
     1 'karuta-keyword-face)
    ;; Arrow operator `:-'.
    ("\\(:-\\)" 1 'karuta-arrow-face)
    ;; Query terminator `?'.
    ("\\(\\?\\)" 1 'karuta-query-face)
    ;; Module qualifiers: the `foo' and separator in `foo:bar'
    (karuta--match-qualifier (1 'karuta-module-qualifier-face)
                             (2 'karuta-module-qualifier-face))
    ;; Variables: X, Out, _Tail, _.
    ("\\_<\\([A-Z_][a-zA-Z0-9_]*\\)\\_>" 1 'karuta-variable-face)
    ;; Predicate / functor names: an atom directly applied to arguments.
    ("\\([a-z][a-zA-Z0-9_-]*\\)[[(]" 1 'karuta-predicate-face)
    ;; Range operator `..'.
    ("\\(\\.\\.\\)" 1 'karuta-operator-face)
    ;; Block braces.
    ("\\([{}]\\)" 1 'karuta-brace-face)
    ;; Argument / list brackets.
    ("\\([][()]\\)" 1 'karuta-bracket-face)
    ;; Separators.
    ("\\([,|]\\)" 1 'karuta-operator-face)
    ;; Remaining plain atoms (constants like nil, debug, this).
    ("\\_<\\([a-z][a-zA-Z0-9_-]*\\)\\_>" 1 'karuta-atom-face)
    ;; Integers (incl. negatives): 5, -1.
    ("\\(-?[0-9]+\\)" 1 'karuta-number-face))
  "Font-lock rules for `karuta-mode'.")

;;;; Indentation

(defun karuta--prev-code-end ()
  "Return the position after the last code char on the previous nonblank line.
Trailing whitespace and `%' line comments are ignored.  Returns nil when
there is no previous code line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        nil
      (forward-line -1)
      (while (and (not (bobp)) (looking-at "[ \t]*$"))
        (forward-line -1))
      (if (looking-at "[ \t]*$")
          nil
        (let ((bol (line-beginning-position))
              (eol (line-end-position))
              (code-end nil))
          (goto-char bol)
          ;; Walk forward; stop at the first `%' that opens a comment.
          (setq code-end eol)
          (while (re-search-forward "%" eol t)
            (when (nth 4 (syntax-ppss (point)))
              (setq code-end (1- (point)))
              (goto-char eol)))
          (goto-char code-end)
          (skip-chars-backward " \t")
          (and (> (point) bol) (point)))))))

(defun karuta--prev-line-continues-p ()
  "Return non-nil when the previous code line leaves a clause open.
That is, it ends with the arrow `:-' or a conjunction comma `,'."
  (let ((end (karuta--prev-code-end)))
    (when end
      (let ((c (char-before end)))
        (or (eq c ?,)
            (and (eq c ?-)
                 (eq (char-before (1- end)) ?:)))))))

(defun karuta--calculate-indent ()
  "Compute the indentation column for the current line."
  (save-excursion
    (beginning-of-line)
    (let* ((ppss (syntax-ppss (point)))
           (depth (car ppss))
           (base (* karuta-indent-offset (max 0 depth))))
      (cond
       ;; A line that opens with a closing bracket/brace dedents one level.
       ((looking-at "[ \t]*[]})]")
        (max 0 (- base karuta-indent-offset)))
       ;; Continuation of a clause body gets one extra level.
       ((karuta--prev-line-continues-p)
        (+ base karuta-indent-offset))
       (t base)))))

(defun karuta-indent-line ()
  "Indent the current line as Karuta code."
  (interactive)
  (let ((target (karuta--calculate-indent))
        (offset (- (current-column) (current-indentation))))
    (indent-line-to target)
    (when (> offset 0)
      (forward-char offset))))

;;;; Compilation

(defun karuta--compile-command ()
  "Return the shell command used to compile the current buffer."
  (format "%s %s %s"
          karuta-executable
          karuta-compile-subcommand
          (if buffer-file-name
              (shell-quote-argument buffer-file-name)
            "source")))

(defun karuta-compile ()
  "Compile the current Karuta file with `karuta compile'."
  (interactive)
  (compile (karuta--compile-command)))

;;;; Imenu

(defconst karuta-imenu-generic-expression
  '(("Predicates"
     "^\\(@?'[^'\n]+'\\|@?[a-z][a-zA-Z0-9_-]*\\)[[(]" 1))
  "Imenu configuration: index clause heads (top-level predicate definitions).")

;;;; Keymap

(defvar karuta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'karuta-compile)
    map)
  "Keymap for `karuta-mode'.")

;;;; Mode definition

;;;###autoload
(define-derived-mode karuta-mode prog-mode "Karuta"
  "Major mode for editing Karuta source and Sakura database files.

\\{karuta-mode-map}"
  :group 'karuta
  :syntax-table karuta-mode-syntax-table
  ;; Comments.
  (setq-local comment-start "% ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?:%+\\|#%\\)[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function
              #'karuta-syntax-propertize-function)
  ;; Font lock.
  (setq-local font-lock-defaults
              '(karuta-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . karuta-syntactic-face-function)))
  ;; Indentation.
  (setq-local indent-line-function #'karuta-indent-line)
  (setq-local electric-indent-chars
              (append '(?\} ?\] ?\)) electric-indent-chars))
  ;; Navigation.
  (setq-local imenu-generic-expression karuta-imenu-generic-expression)
  ;; Compilation.
  (setq-local compile-command (karuta--compile-command)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.krt\\'" . karuta-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.skr\\'" . karuta-mode))

(provide 'karuta-mode)
;;; karuta-mode.el ends here
