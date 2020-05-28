;;; gdscript-godot.el --- Open and run projects in Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Franco Eusébio Garcia <francoegarcia@outlook.com>, Nathan Lovato <nathan@gdquest.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Mar 2020
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
;;  Open files, projects, and run commands in the Godot engine or editor.
;;  This package contains commands that runs Godot processes.
;;
;;; Code:

(require 'gdscript-customization)
(require 'gdscript-utils)

;;;###autoload
(defvar gdscript-godot-debug-selected-option 1)

(defvar gdscript-godot-debug-options-alist
  '((1 . "")
    (2 . "--debug-collisions")
    (3 . "--debug-navigation")
    (4 . "--debug-collisions --debug-navigation")))

(defun gdscript-godot--run-command (cmd &optional show)
  "Run a Godot process.

CMD is the command to be invoked by the shell.  If SHOW, the
output of the process will be provided in a buffer named
`*godot*'."
  (start-process-shell-command "Godot Process" (if show
                                                   "*godot*" nil) cmd))

(defun gdscript-godot--build-shell-command (&optional path)
  "Build a shell command to with the Godot executable.

If PATH is not provided, try to find it using the current
file's directory as starting point."
  (let* ((project-path (or path (gdscript-util--find-project-configuration-file))))
    (concat gdscript-godot-executable " --path " project-path)))

(defun gdscript-godot-open-project-in-editor ()
  "Run Godot Engine Editor."
  (interactive)
  (gdscript-godot--run-command
   (concat (gdscript-godot--build-shell-command) " -e")))

(defun gdscript-godot-run-project ()
  "Run the current project in Godot Engine."
  (interactive)
  (gdscript-godot--run-command
   (gdscript-godot--build-shell-command)))

(defun gdscript-godot-run-project-debug ()
  "Run the current project in Godot Engine.

When run with prefix argument, it offers extra debug options to choose from."
  (interactive)
  (let* ((debug-option-index
          (if current-prefix-arg
              (gdscript-godot-change-debug-options)
            gdscript-godot-debug-selected-option))
         (debug-options (cdr (assoc debug-option-index gdscript-godot-debug-options-alist))))
    (gdscript-godot--run-command
     (concat (gdscript-godot--build-shell-command) " -d " debug-options) t)))

(defun gdscript-godot-run-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-godot--run-command
   (concat (gdscript-godot--build-shell-command) " "
           (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))

(defun gdscript-godot-run-current-scene-debug ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-godot--run-command
   (concat (gdscript-godot--build-shell-command) " -d "
           (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")
   t))

(defun gdscript-godot-edit-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-godot--run-command
   (concat (gdscript-godot--build-shell-command) " -e "
           (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))

(defun gdscript-godot-run-current-script ()
  "Run the current script file in Godot Engine.

For this to work, the script must inherit either from
\"SceneTree\" or \"MainLoop\"."
  (interactive)
  (gdscript-godot--run-command
   (concat (gdscript-godot--build-shell-command) " -s " (file-relative-name buffer-file-name))
   t))

(defun gdscript-godot-debug-options-collection ()
  "List of debug options to choose from by *-read function."
  (list
   (format "1) [%s] <no options>" (if (eq gdscript-godot-debug-selected-option 1) "X" " "))
   (format "2) [%s] %s" (if (eq gdscript-godot-debug-selected-option 2) "X" " ") (cdr (assoc 2 gdscript-godot-debug-options-alist)))
   (format "3) [%s] %s" (if (eq gdscript-godot-debug-selected-option 3) "X" " ") (cdr (assoc 3 gdscript-godot-debug-options-alist)))
   (format "4) [%s] %s" (if (eq gdscript-godot-debug-selected-option 4) "X" " ") (cdr (assoc 4 gdscript-godot-debug-options-alist)))))

(defun gdscript-godot-read-options ()
  "Read debug options preference by user from mini-buffer."
  (cond ((fboundp 'ivy-read)
         (ivy-read "Options: " (gdscript-godot-debug-options-collection)))
        ((fboundp 'ido-completing-read)
         (ido-completing-read "Options: " (gdscript-godot-debug-options-collection)))
        (t
         (completing-read "Options (hit TAB to auto-complete): " (gdscript-godot-debug-options-collection) nil t))))

(defun gdscript-godot-change-debug-options ()
  "Read debug option and parse it as a number.

Once read it is saved in `gdscript-godot-debug-selected-option'
variable for later use."
  (let* ((option (gdscript-godot-read-options))
         (index (string-to-number option)))
    (setq gdscript-godot-debug-selected-option index)))

(provide 'gdscript-godot)
;;; gdscript-godot.el ends here
