;;; gdscript-snippet.el --- Snippets for GDScript  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 GDQuest and contributors

;; Author: John Charman <jchar@mailfence.com>
;; Maintainer: John Charman <jchar@mailfence.com>

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

;; Defines `skeleton's (a.k.a. snippets) for code insertion in GDScript.

;; Also provides the `transient': `gdscript-snippet-menu', for quick access.


;;; Code:

(require 'skeleton)                     ; Built-in
(require 'transient)                    ; Built-in


;; Transient Menu for inserting skeletons

(transient-define-prefix gdscript-snippet-menu ()
  "Menu for GDScript Code Snippets."
  ["GDScript Code Snippets"
   ("f" "function" gdscript-snippet-function)
   ("l" "load" gdscript-snippet-load)
   ("p" "preload" gdscript-snippet-preload)
   ("t" "one-shot timer" gdscript-snippet-timer)
   ("v" "member variable" gdscript-snippet-variable)])


;; Skeleton Definitions

;; str, v1 and v2 are local-variables scoped within `skeleton-insert'.
;; str is set by the interactor (the first argument) if non-nil.
;; v1 and v2 are for memorizing anything you need during expansion.

(define-skeleton gdscript-snippet-function
  "Insert a skeleton for a function"
  ;; interactor
  "function name: "
  ;; skeleton
  (when (y-or-n-p "static?") "static ")
  "func " str "()"
  (progn
    (setq v1 (completing-read "return: " `(,@gdscript-built-in-types
                                           ,@gdscript-built-in-classes)))
    (unless (string-empty-p v1) (skeleton-insert '(nil " -> " str) nil v1)))
  ":" \n >)


(define-skeleton gdscript-snippet-load
  "Inserts a skeleton for loading a file"
  ;; interactor
  nil
  ;; skeleton
  "load(" (gdscript-completion-insert-file-path-at-point) ")")


(define-skeleton gdscript-snippet-preload
  "Inserts a skeleton for preloading a file"
  ;; interactor
  nil
  ;; skeleton
  "preload(" (gdscript-completion-insert-file-path-at-point) ")")


(define-skeleton gdscript-snippet-timer
  "Inserts a skeleton for awaiting a one-shot SceneTreeTimer."
  ;; interactor
  (read-string "duration (default 1.0): " nil nil "1.0")
  ;; skeleton
  "await get_tree().create_timer(" str ").timeout")


(define-skeleton gdscript-snippet-variable
  "Inserts a skeleton for a member variable."
  ;; interactor
  "variable name: "
  ;; skeleton
  "var " str
  (progn
    (setq v1 (completing-read "type: " `(,@gdscript-built-in-types
                                         ,@gdscript-built-in-classes)))
    (unless (string-empty-p v1) (skeleton-insert '(nil ": " str) nil v1)))
  (when (y-or-n-p "getter?")
    (setq v2 t)
    (skeleton-insert '(nil ":" \n "get():" \n "return " str) nil str))
  (when (y-or-n-p "setter?")
    (unless v2 (skeleton-insert '(nil ":")))
    (skeleton-insert '(nil \n "set(v):" \n str " = v") nil str)))


(provide 'gdscript-snippet)
;;; gdscript-snippet.el ends here
