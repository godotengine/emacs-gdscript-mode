;;; gdscript-utils.el --- Utility functions for gdscript-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Jan 2020
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

;; Misc utility functions for GDScript mode.

;;; Code:

(require 'gdscript-syntax)
(require 'gdscript-customization)

(defun gdscript--util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun gdscript--util-forward-comment (&optional direction)
  "Gdscript mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (gdscript-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun gdscript--util-list-directories (directory &optional predicate max-depth)
  "List DIRECTORY subdirs, filtered by PREDICATE and limited by MAX-DEPTH.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files.  When optional argument MAX-DEPTH is non-nil, stop
searching when depth is reached, else don't limit."
  (let* ((dir (expand-file-name directory))
         (dir-length (length dir))
         (predicate (or predicate #'identity))
         (to-scan (list dir))
         (tally nil))
    (while to-scan
      (let ((current-dir (car to-scan)))
        (when (funcall predicate current-dir)
          (setq tally (cons current-dir tally)))
        (setq to-scan (append (cdr to-scan)
                              (gdscript--util-list-files
                               current-dir #'file-directory-p)
                              nil))
        (when (and max-depth
                   (<= max-depth
                       (length (split-string
                                (substring current-dir dir-length)
                                "/\\|\\\\" t))))
          (setq to-scan nil))))
    (nreverse tally)))

(defun gdscript--util-list-files (dir &optional predicate)
  "List files in DIR, filtering with PREDICATE.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files."
  (let ((dir-name (file-name-as-directory dir)))
    (apply #'nconc
           (mapcar (lambda (file-name)
                     (let ((full-file-name (expand-file-name file-name dir-name)))
                       (when (and
                              (not (member file-name '("." "..")))
                              (funcall (or predicate #'identity) full-file-name))
                         (list full-file-name))))
                   (directory-files dir-name)))))


(defun gdscript-util--find-project-configuration-file (&optional start-path)
  "Return the path to the file \"project.godot\".

Start the search from START-PATH if provided. Otherwise, the search
starts from the current buffer path.

WARNING: the Godot project must exist for this function to work."
  (let* ((base-path (or start-path default-directory))
         (dominating-file
          (locate-dominating-file base-path
                                  (lambda (parent)
                                    (directory-files parent t "project.godot")))))
    (when dominating-file (expand-file-name dominating-file))))

(defun gdscript-util--get-godot-project-name ()
  "Retrieve the project name from Godot's configuration file."
  (with-temp-buffer
    (insert-file-contents (concat (gdscript-util--find-project-configuration-file) "project.godot"))
    (goto-char (point-min))
    (if (re-search-forward "config/name=\"\\([^\"]*\\)\"" nil t)
        (match-string 1)
      (error "Could not find the name of the project"))))

(defun gdscript-util--get-godot-buffer-name (&optional editor)
  "Return buffer name for godot's stdout/stderr output."
  (format (if editor "*godot - %s - Editor*" "*godot - %s*") (gdscript-util--get-godot-project-name)))

(defun gdscript-util--get-gdformat-buffer-name ()
  "Return buffer name for godot's stdout/stderr output."
  (format "*gdformat - %s*" (gdscript-util--get-godot-project-name)))

(defun gdscript-util--get-godot-project-file-path-relative (file-path)
  "Return the relative path of `FILE-PATH' to Godot's configuration file."
  (let ((project-configuration-file (gdscript-util--find-project-configuration-file)))
    (when project-configuration-file
      (concat (file-name-sans-extension
               (file-relative-name file-path project-configuration-file))))))

(defun gdscript-util--flatten (xs)
  "Flatten deeply nested list.

For example:
> (gdscript-util--flatten (list 1 2 (list 3 (list 4 5)) nil))
> (1 2 3 4 5)
"
  (cond
   ((null xs) nil)
   ((listp xs) (append (gdscript-util--flatten (car xs)) (gdscript-util--flatten (cdr xs))))
   (t (list xs))))

(defun gdscript-util--read (items &optional prompt)
  "Let's choose single item from ITEMS from mini-buffer.
PROMPT is prompt for read command. Return `nil' if user aborts."
  (let* ((p (if prompt prompt "Options"))
    (result (cond ((and (featurep 'projectile) )
           (projectile-completing-read (format "%s: " p) items))
          ((fboundp 'ivy-read)
           (ivy-read (format "%s: " p) items))
          ((fboundp 'ido-completing-read)
           (ido-completing-read (format "%s: " p) items))
          (t
           (completing-read (format "%s (hit TAB to auto-complete): " p) items nil t)))))
    (if quit-flag nil result)))

(defmacro gdscript-util--with-available-hydra (&rest body)
  ""
  `(progn
     (when (not (featurep 'hydra))
       (error "No `hydra.el' available.  To execute `gdscript-hydra-show' command you need to install hydra.el"))
     ,@body))

(provide 'gdscript-utils)

;;; gdscript-utils.el ends here
