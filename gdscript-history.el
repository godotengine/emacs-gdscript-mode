;;; gdscript-history.el --- History -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: June 2020
;; Keywords: languages
;;
;; This file is not part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;;  Keep history of commands for later quick execution.
;;
;;; Code:

(require 'gdscript-utils)

(defvar gdscript-history--previous-arguments-plist nil
  "Holds history of commands per project.")

(defmacro gdscript-history--with-project-history (project-history &rest body)
  "Execute the forms in BODY with PROJECT-HISTORY being set.

PROJECT-HISTORY will be history of current project."
  (declare (indent 1) (debug t))
  `(let* ((property (gdscript-util--find-project-configuration-file))
          (,project-history (lax-plist-get gdscript-history--previous-arguments-plist property)))
     ,@body))

(defun gdscript-history--add-to-history (arguments)
  "Add ARGUMENTS to history for current project."
  (gdscript-history--with-project-history history
    (push arguments history)
    (delete-dups history)
    (setq gdscript-history--previous-arguments-plist
          (lax-plist-put gdscript-history--previous-arguments-plist property history))))

(defun gdscript-history--last-command ()
  "Return last command from history."
  (gdscript-history--with-project-history history
    (car history)))

(defun gdscript-history--line-data (command)
  "Pretty print COMMAND."
  (let* ((flat (gdscript-util--flatten command))
         (l (car (last flat))))
    (cond ((string-suffix-p ".tscn" l)
           (progn
             (string-match "\\(.*/\\)?\\(.*\\).tscn$" l)
             (list (format "Scene %s.tscn: %s " (match-string-no-properties 2 l) l) (butlast command))))
          ((string-suffix-p ".gd" l)
           (progn
             (string-match "\\(.*/\\)?\\(.*\\).gd$" l)
             (list (format "Script %s.gd: %s " (match-string-no-properties 2 l) l) (butlast command))))
          (t
           (list (format "Project %s: " (gdscript-util--get-godot-project-name)) command)))))

(defun gdscript-history--line (command idx)
  "Render COMMAND as string starting with index IDX."
  (let* ((index (number-to-string (1+ idx)))
         (data (gdscript-history--line-data command))
         (name (car data))
         (args (cadr data)))
    (string-trim (concat index ") " name (mapconcat 'identity args " ")))))

(defun gdscript-history--select-from-history ()
  "Choose command from history."
  (gdscript-history--with-project-history history
    (let* ((history-data (seq-map-indexed #'gdscript-history--line history))
           (selected (gdscript-util--read history-data "Run again"))
           (index (string-to-number selected)))
      (nth (1- index) history))))

(provide 'gdscript-history)

;;; gdscript-history.el ends here
