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
    (modify-syntax-entry ?# "." table) ; see `karuta-syntax-propertize-function'
    ;; Punctuation
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?. "." table) ; see `karuta-syntax-propertize-function'
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    table)
  "Syntax table for `karuta-mode'.")

(defun karuta--expression-end (start)
  (save-excursion
    (goto-char start)
    (forward-sexp)
    (while (looking-at ":")
      (forward-sexp))
    (when (looking-at "[[(]")
      (forward-sexp))
    (1- (point))))

(defun karuta--inside-comment-p (pos)
  (save-excursion
    (nth 4 (parse-partial-sexp (point-min) pos))))

(defun karuta--propertize-expression-comment ()
  (let ((start (match-beginning 0))
        (end (karuta--expression-end (match-end 0))))
    (unless (karuta--inside-comment-p start)
      (put-text-property start (1+ start) 'syntax-table (string-to-syntax "!"))
      (put-text-property end (1+ end) 'syntax-table (string-to-syntax "!")))))

(defun karuta--propertize-line-comment ()
  (unless (eql ?# (char-before (match-beginning 0)))
    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "<"))))

(defun karuta--propertize-dot ()
  (condition-case nil
      (save-excursion
        (goto-char (scan-lists (match-beginning 0) -1 1))
        (string-to-syntax (if (looking-at ":-") ")" ".")))
    (scan-error (string-to-syntax "."))))

(defalias 'karuta-syntax-propertize-function
  (syntax-propertize-rules
   ;; Disallow strings spanning multiple lines
   ("\\(\"\\)\\(?:[^\"\n]*\\)\\(\n\\)" (1 "|") (2 "|"))
   ("\\(\"\\)\\(?:[^\"\n]*\\)\\(\"\\)" (1 "\"") (2 "\""))
   ;; Ditto for atoms
   ("\\('\\)\\(?:[^'\n]*\\)\\(\n\\)" (1 "|") (2 "|"))
   ("\\('\\)\\(?:[^'\n]*\\)\\('\\)" (1 "\"") (2 "\""))
   ;; Arrow and dot
   ("\\(:\\)-" (1 "(."))
   ("\\." (0 (karuta--propertize-dot)))
   ;; Comments
   ;; Line comments are highlighted using `syntax-propertize-function'
   ;; rather than the syntax table because the syntax table properties
   ;; are applied before this gets to run, which would mean that lines
   ;; containing expression comments would be first read as containing
   ;; line comments (due to the % on #%), which breaks skipping exprs,
   ;; and, thus, `karuta--propertize-expression-comment'.
   ("%" (0 (karuta--propertize-line-comment)))
   ("#%" (0 (karuta--propertize-expression-comment)))))

(defun karuta--quoted-atom-face (start)
  (save-excursion
    (goto-char start)
    (forward-sexp)
    (cond ((looking-at-p "[[(]") 'karuta-predicate-face)
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
  ;; TODO drop the regex in favour of using the syntax table
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

(defun karuta--calculate-indent ()
  (* karuta-indent-offset
     (save-excursion
       (beginning-of-line)
       (let ((level (car (syntax-ppss (point)))))
         (back-to-indentation)
         (if (or (eql 5 (syntax-class (syntax-after (point)))) (looking-at "{"))
             (1- level)
           level)))))

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
