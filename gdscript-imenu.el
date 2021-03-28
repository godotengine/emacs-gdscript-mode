;;; gdscript-imenu.el --- Imenu support for GDScript -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Feb 2020
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Populates imenu with a list of headings: functions and subclasses in the
;; current GDScript buffer.

;;; Code:

(require 'gdscript-indent-and-nav)

(defvar gdscript-imenu-format-item-label-function
  'gdscript-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar gdscript-imenu-format-parent-item-label-function
  'gdscript-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar gdscript-imenu-format-parent-item-jump-label-function
  'gdscript-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun gdscript-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun gdscript-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (gdscript-imenu-format-item-label type name)))

(defun gdscript-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))

(defun gdscript-imenu--get-defun-type-name ()
  "Return defun type and name at current position."
  (when (looking-at gdscript-nav-beginning-of-defun-regexp)
    (let ((split (split-string (match-string-no-properties 0))))
      (if (= (length split) 2)
          split
        (list (concat (car split) " " (cadr split))
              (car (last split)))))))

(defun gdscript-imenu--put-parent (type name pos tree)
  "Add the parent with TYPE, NAME and POS to TREE."
  (let ((label
         (funcall gdscript-imenu-format-item-label-function type name))
        (jump-label
         (funcall gdscript-imenu-format-parent-item-jump-label-function type name)))
    (if (not tree)
        (cons label pos)
      (cons label (cons (cons jump-label pos) tree)))))

(defun gdscript-imenu--build-tree (&optional min-indent prev-indent tree)
  "Recursively build the tree of nested definitions of a node.
Arguments MIN-INDENT, PREV-INDENT and TREE are internal and should
not be passed explicitly unless you know what you are doing."
  (setq min-indent (or min-indent 0)
        prev-indent (or prev-indent gdscript-indent-offset))
  (let* ((pos (gdscript-nav-backward-defun))
         (defun-type-name (and pos (gdscript-imenu--get-defun-type-name)))
         (type (car defun-type-name))
         (name (cadr defun-type-name))
         (label (when name
                  (funcall gdscript-imenu-format-item-label-function type name)))
         (indent (current-indentation))
         (children-indent-limit (+ gdscript-indent-offset min-indent)))
    (cond ((not pos)
           ;; Nothing found, probably near to bobp.
           nil)
          ((<= indent min-indent)
           ;; The current indentation points that this is a parent
           ;; node, add it to the tree and stop recursing.
           (gdscript-imenu--put-parent type name pos tree))
          (t
           (gdscript-imenu--build-tree
            min-indent
            indent
            (if (<= indent children-indent-limit)
                ;; This lies within the children indent offset range,
                ;; so it's a normal child of its parent (i.e., not
                ;; a child of a child).
                (cons (cons label pos) tree)
              ;; Oh no, a child of a child?!  Fear not, we
              ;; know how to roll.  We recursively parse these by
              ;; swapping prev-indent and min-indent plus adding this
              ;; newly found item to a fresh subtree.  This works, I
              ;; promise.
              (cons
               (gdscript-imenu--build-tree
                prev-indent indent (list (cons label pos)))
               tree)))))))

(defun gdscript-imenu-create-index ()
  "Return tree Imenu alist for the current Gdscript buffer.
Change `gdscript-imenu-format-item-label-function',
`gdscript-imenu-format-parent-item-label-function',
`gdscript-imenu-format-parent-item-jump-label-function' to
customize how labels are formatted."
  (goto-char (point-max))
  (let ((index)
        (tree))
    (while (setq tree (gdscript-imenu--build-tree))
      (setq index (cons tree index)))
    index))

(defun gdscript-imenu-create-flat-index (&optional alist prefix)
  "Return flat outline of the current Gdscript buffer for Imenu.
Optional argument ALIST is the tree to be flattened; when nil
`gdscript-imenu-build-index' is used with
`gdscript-imenu-format-parent-item-jump-label-function'
`gdscript-imenu-format-parent-item-label-function'
`gdscript-imenu-format-item-label-function' set to
  (lambda (type name) name)
Optional argument PREFIX is used in recursive calls and should
not be passed explicitly.

Converts this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\"
      (\"decorator\" . 173)
      (\"wrap\"
       (\"wrap\" . 353)
       (\"wrapped_f\" . 393))))

To this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\" . 173)
     (\"decorator.wrap\" . 353)
     (\"decorator.wrapped_f\" . 393))"
  ;; Inspired by imenu--flatten-index-alist removed in revno 21853.
  (apply
   'nconc
   (mapcar
    (lambda (item)
      (let ((name (if prefix
                      (concat prefix "." (car item))
                    (car item)))
            (pos (cdr item)))
        (cond ((or (numberp pos) (markerp pos))
               (list (cons name pos)))
              ((listp pos)
               (cons
                (cons name (cdar pos))
                (gdscript-imenu-create-flat-index (cddr item) name))))))
    (or alist
        (let* ((fn (lambda (_type name) name))
               (gdscript-imenu-format-item-label-function fn)
               (gdscript-imenu-format-parent-item-label-function fn)
               (gdscript-imenu-format-parent-item-jump-label-function fn))
          (gdscript-imenu-create-index))))))

(provide 'gdscript-imenu)

;;; gdscript-imenu.el ends here
