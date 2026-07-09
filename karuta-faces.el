;;; karuta-faces.el --- Colorful faces for Karuta mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Marcos Magueta

;; Author: Marcos Magueta <maguetamarcos@gmail.com>
;; Keywords: languages, faces
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file defines one face per *morphological category* of the Karuta
;; language.  The design goal is a very colorful theme in which every kind
;; of token is instantly distinguishable by hue:
;;
;;   Category            Face                              Hue
;;   -----------------   -------------------------------   -----------
;;   Directive keywords  `karuta-keyword-face'             purple
;;   Predicate/functor   `karuta-predicate-face'           blue
;;   Plain atom          `karuta-atom-face'                cyan
;;   Quoted atom         `karuta-quoted-atom-face'         green
;;   Variable            `karuta-variable-face'            gold
;;   Integer             `karuta-number-face'              orange
;;   Comment             `karuta-comment-face'             gray
;;   Neck `:-'           `karuta-neck-face'                red
;;   Query `?'           `karuta-query-face'               magenta
;;   Sakura `@' relation `karuta-sakura-relation-face'     pink
;;   Module qualifier    `karuta-module-qualifier-face'    violet
;;   Brackets `[] ()'    `karuta-bracket-face'             light gray
;;   Braces `{}'         `karuta-brace-face'               purple (bold)
;;   Separators `, . |'  `karuta-operator-face'            dim
;;
;; Each face carries an explicit palette for both dark and light
;; backgrounds so the colors stay legible either way.

;;; Code:

(defgroup karuta-faces nil
  "Faces for `karuta-mode', one per morphological category."
  :group 'karuta
  :prefix "karuta-")

(defface karuta-keyword-face
  '((((background dark))  (:foreground "#c678dd" :weight bold))
    (((background light)) (:foreground "#8e44ad" :weight bold))
    (t (:foreground "#c678dd" :weight bold)))
  "Face for Karuta directive keywords (module, signature, import, ...)."
  :group 'karuta-faces)

(defface karuta-predicate-face
  '((((background dark))  (:foreground "#61afef"))
    (((background light)) (:foreground "#2563eb"))
    (t (:foreground "#61afef")))
  "Face for predicate / functor names (an atom applied to arguments)."
  :group 'karuta-faces)

(defface karuta-atom-face
  '((((background dark))  (:foreground "#56b6c2"))
    (((background light)) (:foreground "#0e7490"))
    (t (:foreground "#56b6c2")))
  "Face for plain atoms used as constants (not applied to arguments)."
  :group 'karuta-faces)

(defface karuta-quoted-atom-face
  '((((background dark))  (:foreground "#98c379"))
    (((background light)) (:foreground "#16a34a"))
    (t (:foreground "#98c379")))
  "Face for quoted atoms, e.g. \\='factorial loop\\='."
  :group 'karuta-faces)

(defface karuta-variable-face
  '((((background dark))  (:foreground "#e5c07b"))
    (((background light)) (:foreground "#b45309"))
    (t (:foreground "#e5c07b")))
  "Face for logic variables, e.g. X, Out, NewAcc, _."
  :group 'karuta-faces)

(defface karuta-number-face
  '((((background dark))  (:foreground "#d19a66"))
    (((background light)) (:foreground "#c2410c"))
    (t (:foreground "#d19a66")))
  "Face for integer literals."
  :group 'karuta-faces)

(defface karuta-comment-face
  '((((background dark))  (:foreground "#7f848e" :slant italic))
    (((background light)) (:foreground "#6b7280" :slant italic))
    (t (:foreground "#7f848e" :slant italic)))
  "Face for comments (`%' line comments and `#%' expression comments)."
  :group 'karuta-faces)

(defface karuta-neck-face
  '((((background dark))  (:foreground "#e06c75" :weight bold))
    (((background light)) (:foreground "#dc2626" :weight bold))
    (t (:foreground "#e06c75" :weight bold)))
  "Face for the neck operator `:-' (\"holds\")."
  :group 'karuta-faces)

(defface karuta-query-face
  '((((background dark))  (:foreground "#d670d6" :weight bold))
    (((background light)) (:foreground "#a21caf" :weight bold))
    (t (:foreground "#d670d6" :weight bold)))
  "Face for the query operator `?'."
  :group 'karuta-faces)

(defface karuta-sakura-relation-face
  '((((background dark))  (:foreground "#ff79c6" :weight bold))
    (((background light)) (:foreground "#db2777" :weight bold))
    (t (:foreground "#ff79c6" :weight bold)))
  "Face for Sakura database relations, e.g. @inc, @square."
  :group 'karuta-faces)

(defface karuta-module-qualifier-face
  '((((background dark))  (:foreground "#c586c0" :slant italic))
    (((background light)) (:foreground "#7c3aed" :slant italic))
    (t (:foreground "#c586c0" :slant italic)))
  "Face for module qualifiers, e.g. the `karuta:' in `karuta:nat'."
  :group 'karuta-faces)

(defface karuta-bracket-face
  '((((background dark))  (:foreground "#abb2bf"))
    (((background light)) (:foreground "#334155"))
    (t (:foreground "#abb2bf")))
  "Face for argument/list brackets `[' `]' `(' `)'."
  :group 'karuta-faces)

(defface karuta-brace-face
  '((((background dark))  (:foreground "#c678dd" :weight bold))
    (((background light)) (:foreground "#7c3aed" :weight bold))
    (t (:foreground "#c678dd" :weight bold)))
  "Face for block braces `{' `}'."
  :group 'karuta-faces)

(defface karuta-operator-face
  '((((background dark))  (:foreground "#828997"))
    (((background light)) (:foreground "#64748b"))
    (t (:foreground "#828997")))
  "Face for separators and operators, e.g. `,' `.' `|' `..'."
  :group 'karuta-faces)

(provide 'karuta-faces)
;;; karuta-faces.el ends here
