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
;; Tree-sitter mode for Gdscript.(Refer to python-ts-mode)
;; That supports the use tree-sitter for font-lock, imenu, indentation,
;; and navigation of Gdscript files.

;;
;;; Code:

(when (version< "29" emacs-version)
  (require 'treesit))


;;; Imenu

(defvar gdscript-ts-imenu-format-item-label-function
  'gdscript-ts-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar gdscript-ts-imenu-format-parent-item-label-function
  'gdscript-ts-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar gdscript-ts-imenu-format-parent-item-jump-label-function
  'gdscript-ts-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun gdscript-ts-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun gdscript-ts-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (gdscript-ts-imenu-format-item-label type name)))

(defun gdscript-ts-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))


;;; Keywords

(defvar gdscript-ts--keyword-regex
  (rx bot (| "func" "var" "const" "set" "get" "setget" "signal" "extends"
             "match" "if" "elif" "else" "for" "in" "while" "break" "continue" 
             "pass" "return" "when" "yield" "await"
             "class" "class_name" "abstract" "is" "onready" "tool" "static"
             "export" "as" "void" "enum" "assert" "breakpoint"
             "sync" "remote" "master" "puppet"
             "remotesync" "mastersync" "puppetsync"
             "trait" "namespace" "super"
             "and" "or" "not"
             "await" "yield" "self") eot))

;;; Types

(defvar gdscript-ts--builtin-type-regex
  "\\`\\(int\\|bool\\|float\\|void\\|Vector2\\|Vector2i\\|Vector3\\|Vector3i\\|Vector4\\|Vector4i\\|Color\\|Rect2\\|Rect2i\\|Array\\|Basis\\|Dictionary\\|Plane\\|Quat\\|RID\\|Rect3\\|Transform\\|Transform2D\\|Transform3D\\|AABB\\|String\\|Color\\|NodePath\\|PoolByteArray\\|PoolIntArray\\|PoolRealArray\\|PoolStringArray\\|PoolVector2Array\\|PoolVector3Array\\|PoolColorArray\\|bool\\|int\\|float\\|Signal\\|Callable\\|StringName\\|Quaternion\\|Projection\\|PackedByteArray\\|PackedInt32Array\\|PackedInt64Array\\|PackedFloat32Array\\|PackedFloat64Array\\|PackedStringArray\\|PackedVector2Array\\|PackedVector2iArray\\|PackedVector3Array\\|PackedVector3iArray\\|PackedVector4Array\\|PackedColorArray\\|JSON\\|UPNP\\|OS\\|IP\\|JSONRPC\\|XRVRS\\)\\'")

(defvar gdscript-ts--type-regex
  "\\`[A-Z][a-zA-Z0-9_]*[a-z][a-zA-Z0-9_]*\\'")

;;; Constants

(defvar gdscript-ts--constant-regex "\\`[A-Z_][A-Z0-9_]+\\'")

;;; Setting

(defvar gdscript-ts--feature-list
  '(( comment definition)
    ( keyword string type annotation)
    ( number constant escape-sequence)
    ( bracket delimiter function operator property)))

(defvar gdscript-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'gdscript
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'gdscript
   :feature 'constant
   `(([(null) (false) (true)] @font-lock-constant-face)
     (const_statement name: (name) @font-lock-constant-face)
     (enumerator left: (identifier) @font-lock-constant-face)
     ((identifier) @font-lock-constant-face
      (:match ,gdscript-ts--constant-regex @font-lock-constant-face))
     (variable_statement
      name: (name) @font-lock-constant-face
      (:match ,gdscript-ts--constant-regex @font-lock-constant-face)))

   :language 'gdscript
   :feature 'bracket
   `(["[" "]" "(" ")" "{" "}"] @font-lock-bracket-face)

   :language 'gdscript
   :feature 'delimiter
   `(["," ":" "."] @font-lock-delimiter-face)

   :language 'gdscript
   :feature 'type
   `(((identifier) @font-lock-builtin-face
      (:match ,gdscript-ts--builtin-type-regex @font-lock-builtin-face))
     (get_node) @font-lock-builtin-face
     ((identifier) @font-lock-type-face
      (:match ,gdscript-ts--type-regex @font-lock-type-face))
     (enum_definition name: (_) @font-lock-type-face)
     (class_name_statement (name) @font-lock-type-face)
     (class_definition (name) @font-lock-type-face))

   :language 'gdscript
   :feature 'definition
   '((function_definition (name) @font-lock-function-name-face))

   :language 'gdscript
   :feature 'annotation
   '((annotation "@" @font-lock-preprocessor-face
                 (identifier) @font-lock-preprocessor-face))

   :language 'gdscript
   :feature 'keyword
   `((ERROR _ @font-lock-keyword-face (:match ,gdscript-ts--keyword-regex @font-lock-keyword-face))
     (_ _ @font-lock-keyword-face (:match ,gdscript-ts--keyword-regex @font-lock-keyword-face)))

   :language 'gdscript
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'gdscript
   :feature 'function
   '((call (identifier) @font-lock-builtin-face (:match "preload" @font-lock-builtin-face))
     (call (identifier) @font-lock-function-call-face)
     (attribute_call (identifier) @font-lock-function-call-face))

   :language 'gdscript
   :feature 'number
   '(([(integer) (float)] @font-lock-number-face))

   :language 'gdscript
   :feature 'property
   '((attribute (identifier) (identifier) @font-lock-property-use-face))

   :feature 'operator
   :language 'gdscript
   `(["+" "+="   "-" "-=" "*" "*=" "/" "/=" "^"  "^="  ">"  ">="
      "<" "<="   "|" "|=" "%" "%=" "&" "&=" ">>" ">>=" "<<" "<<="
      "||" "&&" "==" "!=" "->" "~" "=" ":="]
     @font-lock-operator-face)

   :language 'gdscript
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)))


;;; Funtion

(defun gdscript-ts--treesit-defun-name (node)
  "Return the defun name of NODE."
  (treesit-node-text (treesit-search-subtree node "^name$" nil t) t))

(defun gdscript-ts--imenu-treesit-create-index-1 (node)
  "Given a sparse tree, create an imenu alist.

NODE is the root node of the tree returned by
`treesit-induce-sparse-tree' (not a tree-sitter node, its car is
a tree-sitter node).  Walk that tree and return an imenu alist.

Return a list of ENTRY where

ENTRY := (NAME . MARKER)
       | (NAME . ((JUMP-LABEL . MARKER)
                  ENTRY
                  ...)

NAME is the function/class's name, JUMP-LABEL is like \"*function
definition*\"."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'gdscript-ts--imenu-treesit-create-index-1
                           children))
         (type (pcase (treesit-node-type ts-node)
                 ("function_definition" 'def)
                 ("export_variable_statement" 'e-var)
                 ("onready_variable_statement" 'o-var)
                 ("variable_statement" 'var)
                 ("class_definition" 'class)))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (or (treesit-defun-name ts-node)
                     "Anonymous")))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node)
      subtrees)
     (subtrees
      (let ((parent-label
             (funcall gdscript-ts-imenu-format-parent-item-label-function
                      type name))
            (jump-label
             (funcall
              gdscript-ts-imenu-format-parent-item-jump-label-function
              type name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     (t (let ((label
               (funcall gdscript-ts-imenu-format-item-label-function
                        type name)))
          (list (cons label marker)))))))

(defun gdscript-ts-imenu-treesit-create-index (&optional node)
  "Return tree Imenu alist for the current Gdscript buffer.

Change `gdscript-ts-imenu-format-item-label-function',
`gdscript-ts-imenu-format-parent-item-label-function',
`gdscript-ts-imenu-format-parent-item-jump-label-function' to
customize how labels are formatted.

NODE is the root node of the subtree you want to build an index
of.  If nil, use the root node of the whole parse tree.

Similar to `gdscript-imenu-create-index' but use tree-sitter."
  (let* ((node (or node (treesit-buffer-root-node 'gdscript)))
         (tree (treesit-induce-sparse-tree
                node
                (rx (or (seq bol
                             (or "onready_" "export_" "")
                             "variable_statement"
                             eol)
                        (seq bol
                             (or "function" "class")
                             "_definition"
                             eol)))
                nil 1000)))
    (gdscript-ts--imenu-treesit-create-index-1 tree)))

;;;###autoload
(define-derived-mode gdscript-ts-mode gdscript-mode "Gdscript"
  "Major mode for editing gdscript files, using tree-sitter library.

\\{gdscript-ts-mode-map}"
  :syntax-table gdscript-mode-syntax-table
  (when (treesit-ready-p 'gdscript)
    (treesit-parser-create 'gdscript)
    (setq-local treesit-font-lock-feature-list gdscript-ts--feature-list)
    (setq-local treesit-font-lock-settings gdscript-ts--treesit-settings)
    ;;; TODO: create-imenu
    (setq-local imenu-create-index-function
                #'gdscript-ts-imenu-treesit-create-index)
    (setq-local treesit-defun-type-regexp (rx (seq bol
                                                   (or "function" "class")
                                                   "_definition"
                                                   eol)))
    (setq-local treesit-defun-name-function
                #'gdscript-ts--treesit-defun-name)
    (treesit-major-mode-setup)


    (add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-ts-mode))
    (add-to-list 'interpreter-mode-alist '("gdscript[0-9.]*" . gdscript-ts-mode))))

(provide 'gdscript-ts-mode)
;;; gdscript-ts-mode.el ends here
