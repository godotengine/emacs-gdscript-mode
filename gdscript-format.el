;;; gdscript-format.el --- GDScript formatting with gdformat -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Pawel Lampe

;; Author: Pawel Lampe <pawel.lampe@gmail.com>, Nathan Lovato <nathan@gdquest.com>
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

;; Format a buffer containing GDScript code with the gdformat GDScript formatter.
;; This code is derived from gdtoolkit, see https://github.com/Scony/godot-gdscript-toolkit

;;; Code:

(require 'gdscript-comint-gdformat)

(defmacro gdscript-format--save-buffer (&rest body)
  "Execute the forms in BODY if current buffer is gdscript.

It also activates `auto-revert-mode' and saves the buffer if is it modified."
  (declare (indent 1) (debug t))
  `(when (and (buffer-file-name)
              (string-match ".*.gd$" (buffer-file-name)))
     (unless (bound-and-true-p auto-revert-mode)
       (auto-revert-mode))
     (when (buffer-modified-p)
       (save-buffer))
     ,@body))

(defmacro gdscript-format--with-gdscripts (gdscript-buffers &rest body)
  "Execute the forms in BODY with GDSCRIPT-BUFFERS containing all gdscript buffers currently open."
  (declare (indent 1) (debug t))
  `(progn
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
         (gdscript-format--save-buffer
             (push (buffer-file-name) ,gdscript-buffers))))
     ,@body))

(defun gdscript-format--format-region (start end)
  "Format the region between START and END using `gdformat'."
  (let
      ((cmd (concat "echo " (shell-quote-argument (buffer-substring start end)) "|" gdscript-gdformat-executable " -"))
       (error-buffer "*gdformat-errors*"))
    (if (eq (with-temp-buffer (call-process-shell-command cmd)) 0)
        (save-excursion
          (shell-command-on-region start end cmd (buffer-name) t error-buffer t))
      (progn
        (with-current-buffer error-buffer (erase-buffer))
        (shell-command-on-region start end cmd nil nil error-buffer t)))))

(defun gdscript-format-region()
  "Format the selected region using `gdformat'."
  (interactive)
  (gdscript-format--format-region
   (region-beginning) (region-end)))

(defun gdscript-format-buffer()
  "Format the current buffer using `gdformat'."
  (interactive)
  (gdscript-format--save-buffer
      (gdscript-comint-gdformat--run (list (buffer-file-name)))))

(defun gdscript-comint-gdformat--modified-buffers ()
  "Save and format all modified buffers using `gdformat'."
  (let ((gdscript-buffers))
    (gdscript-format--with-gdscripts gdscript-buffers
      (when gdscript-buffers
        (gdscript-comint-gdformat--run gdscript-buffers)))))

(defun gdscript-format-all()
  "Save modified buffers and then format all gdscripts in the project."
  (interactive)
  (let ((gdscript-buffers))
    (gdscript-format--with-gdscripts gdscript-buffers
      (let* ((rl (gdscript-util--find-project-configuration-file))
             (gdscript-file-list (directory-files-recursively rl ".*.gd$" t)))
        (let ((all-gdscripts (delete-dups (append gdscript-file-list gdscript-buffers))))
          (when all-gdscripts
            (pop-to-buffer (gdscript-comint-gdformat--run all-gdscripts))))))))

(provide 'gdscript-format)

;;; gdscript-format.el ends here
