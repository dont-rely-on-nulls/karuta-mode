;;; karuta-faces.el --- Colorful faces for Karuta mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Marcos Magueta

;; Author: Marcos Magueta <maguetamarcos@gmail.com>
;; Keywords: languages, faces
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file defines one face per *morphological category* of the Karuta
;; language.  The design goal is a very colorful theme in which every kind
;; of token is instantly distinguishable by hue, given an approprite theme.

;;; Code:

(defgroup karuta-faces nil
  "Faces for `karuta-mode', one per morphological category."
  :group 'karuta
  :prefix "karuta-")

(defface karuta-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for Karuta directive keywords (module, signature, import, ...)."
  :group 'karuta-faces)

(defface karuta-predicate-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for predicate / functor names (an atom applied to arguments)."
  :group 'karuta-faces)

(defface karuta-atom-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for plain atoms used as constants (not applied to arguments)."
  :group 'karuta-faces)

(defface karuta-quoted-atom-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for quoted atoms, e.g. \\='factorial loop\\='."
  :group 'karuta-faces)

(defface karuta-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for logic variables, e.g. X, Out, NewAcc, _."
  :group 'karuta-faces)

(defface karuta-number-face
  '((t (:inherit font-lock-constant-face)))
  "Face for integer literals."
  :group 'karuta-faces)

(defface karuta-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for comments (`%' line comments and `#%' expression comments)."
  :group 'karuta-faces)

(defface karuta-neck-face
  '((t (:inherit font-lock-operator-face)))
  "Face for the neck operator `:-' (\"holds\")."
  :group 'karuta-faces)

(defface karuta-query-face
  '((t (:inherit font-lock-operator-face)))
  "Face for the query operator `?'."
  :group 'karuta-faces)

(defface karuta-module-qualifier-face
  '((t (:inherit font-lock-type-face)))
  "Face for module qualifiers, e.g. the `karuta:' in `karuta:nat'."
  :group 'karuta-faces)

(defface karuta-bracket-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for argument/list brackets `[' `]' `(' `)'."
  :group 'karuta-faces)

(defface karuta-brace-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for block braces `{' `}'."
  :group 'karuta-faces)

(defface karuta-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Face for separators and operators, e.g. `,' `.' `|' `..'."
  :group 'karuta-faces)

(provide 'karuta-faces)
;;; karuta-faces.el ends here
