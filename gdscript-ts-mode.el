;;; gdscript-ts-mode.el --- Summary -*- lexical-binding: t -*-

;; Copyright (C) 2023 GDQuest and contributors

;; Author: xiliuya <xiliuya@aliyun.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 0.1.0
;; Maintainer:  xiliuya <xiliuya@aliyun.com>
;; Created: 2023-05-22 19:14:43

;; Keywords: languages
;; Package-Requires: ((emacs "26.3"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tree-sitter mode for Gdscript.
;;
;;; Code:

(require 'treesit)

(defvar gdscript-ts--treesit-keywords '("and" "as" "break" "class" "class_name"
                                        "const" "continue" "elif" "else" "enum" "export" "extends" "for" "func" "if" "in" "is"
                                        "master" "match" "not" "onready" "or" "pass"  "puppet" "remote" "remotesync" "return" "setget" "signal"
                                        "var" "while"))


(defvar gdscript-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'gdscript
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'gdscript
   :feature 'definition
   '((function_definition (name) @font-lock-function-name-face)
     (class_definition
      (name) @font-lock-function-name-face)
     (parameters (identifier) @font-lock-variable-name-face))

   :language 'gdscript
   :feature 'keyword
   `(([,@gdscript-ts--treesit-keywords] @font-lock-keyword-face)
     ([(false) (true)] @font-lock-keyword-face))

   :language 'gdscript
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'gdscript
   :feature 'type
   '(((type) @font-lock-type-face)
     (get_node) @font-lock-type-face)

   :feature 'function
   :language 'gdscript
   '((call (identifier) @font-lock-function-call-face)
     (attribute_call (identifier) @font-lock-function-call-face))

   :language 'gdscript
   :feature 'variable
   '((_ (name) @font-lock-variable-name-face))

   :feature 'number
   :language 'gdscript
   '(([(integer) (float)] @font-lock-number-face))

   :feature 'property
   :language 'gdscript
   '((attribute (identifier) (identifier) @font-lock-property-use-face))


   :feature 'operator
   :language 'gdscript
   `(["+" "-" "*" "/" "^" ">" "<" "="] @font-lock-operator-face)))

(defun gdscript-ts--treesit-defun-name (node)
  "Return the defun name of NODE."
  (treesit-node-text (treesit-search-subtree node "^name$" nil t) t))

;;;###autoload
(define-derived-mode gdscript-ts-mode gdscript-mode "Gdscript"
  "Major mode for editing gdscript files, using tree-sitter library.

\\{gdscript-ts-mode-map}"
  :syntax-table gdscript-mode-syntax-table
  (when (treesit-ready-p 'gdscript)
    (treesit-parser-create 'gdscript)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( function variable number property operator)))
    (setq-local treesit-font-lock-settings gdscript-ts--treesit-settings)
    ;;; TODO: create-imenu
    ;; (setq-local imenu-create-index-function
    ;;             #'gdscript-ts-imenu-treesit-create-index)
    (setq-local treesit-defun-type-regexp (rx (or "func" "class_name")))
    (setq-local treesit-defun-name-function
                #'gdscript-ts--treesit-defun-name)
    (treesit-major-mode-setup)


    (add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-ts-mode))
    (add-to-list 'interpreter-mode-alist '("gdscript[0-9.]*" . gdscript-ts-mode))))

(provide 'gdscript-ts-mode)
;;; gdscript-ts-mode.el ends here
