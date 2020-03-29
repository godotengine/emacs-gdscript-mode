;;; gdscript-utils.el --- Utility functions for gdscript-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
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

;;;###autoload
(defun gdscript--run-command (cmd &optional dedicated show)
  "Run a Godot process.
Input and output via buffer named after
`gdscript--shell-buffer-name'.  If there is a process already
running in that buffer, just switch to it.

With argument, allows you to define CMD so you can edit the
command used to call the interpreter and define DEDICATED, so a
dedicated process for the current buffer is open.  When numeric
prefix arg is other than 0 or 4 do not SHOW."
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Run Godot: " (gdscript--build-shell-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (gdscript--build-shell-command) nil t)))
  (start-process-shell-command "Godot Process" (if show
                                                   gdsript-shell-buffer-name nil)
                               cmd))    ; gdscript-godot-executable))

(defun gdscript--build-shell-command ()
  "Calculate the string used to execute the inferior Godot-GDScript process."
  (format "%s %s"
          (shell-quote-argument
           (executable-find gdscript-godot-executable))
          gdscript-godot-executable-args))

(defun gdscript--build-shell-command (&optional path)
  "Build base shell command to run Godot Engine with the
project's base PATH. If PATH is not provided, try to find it
using the current file's directory as starting point."
  (let* ((project-path (or path (gdscript--find-project-configuration-path))))
    (concat gdscript-godot-executable " --path " project-path)))

(defun gdscript--run-godot-editor ()
  "Run Godot Engine Editor."
  (interactive)
  (gdscript--run-command
   (concat (gdscript--build-shell-command) " -e")))

(defun gdscript--run-project-in-godot ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ((project-path (gdscript--find-project-configuration-path)))
    (gdscript--run-command
     (gdscript--build-shell-command))))

(defun gdscript--run-project-in-godot-debug-mode ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ((project-path (gdscript--find-project-configuration-path)))
    (gdscript--run-command
     (concat (gdscript--build-shell-command) " -d"))))

(defun gdscript--run-current-scene-in-godot ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript--run-command
   (concat (gdscript--build-shell-command) " "
           (gdscript--get-file-relative-path-to-project buffer-file-name) ".tscn")))

(defun gdscript--run-current-scene-in-godot-debug-mode ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript--run-command
   (concat (gdscript--build-shell-command) " -d "
           (gdscript--get-file-relative-path-to-project buffer-file-name) ".tscn")))

(defun gdscript--edit-current-scene-in-godot ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript--run-command
   (concat (gdscript--build-shell-command) " -e "
           (gdscript--get-file-relative-path-to-project buffer-file-name) ".tscn")))

(defun gdscript--run-current-script-in-godot ()
  "Run the current script file in Godot Engine.

For this to work, the script must inherit either from
\"SceneTree\" or \"MainLoop\"."
  (interactive)
  (gdscript--run-command
   (concat (gdscript--build-shell-command) " -s " (file-relative-name buffer-file-name))))

(defun gdscript--find-project-configuration-path (&optional path)
  "Return the path where Godot's configuration File (\"project.godot\") is stored.

If PATH is given, starts searching by it. Otherwise, the search
starts by the current buffer path."
  ;; This assumes that the project does exist (i.e. it was created before the
  ;; call). The function will fail if the project is not found.
  (let ((base-path (or path default-directory)))
    (locate-dominating-file base-path
                            (lambda (parent)
                              (directory-files parent t "project.godot")))))

(defun gdscript--get-project-name ()
  "Retrieve the project name from Godot's configuration file."
  (with-temp-buffer
    (insert-file-contents (concat (gdscript--find-project-configuration-path) "project.godot"))
    (goto-char (point-min))
    (if (re-search-forward "config/name=\"\\([^\"]*\\)\"" nil t)
        (match-string 1)
      (error "Could not find the name of the project"))))

(defun gdscript--get-file-relative-path-to-project (file-path)
  "Return the relative path of `file-path' to Godot's configuration file."
  (concat (gdscript--build-shell-command) " -d "
          (file-name-sans-extension
           (file-relative-name file-path
                               (gdscript--find-project-configuration-path)))))

(provide 'gdscript-utils)

;;; gdscript-utils.el ends here
