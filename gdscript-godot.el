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
(defun gdscript-run-command (cmd &optional show)
  "Run a Godot process.
Input and output via buffer named after `*godot*'. If there is a process
already running in that buffer, just switch to it.

With argument, allows you to define CMD so you can edit the
command used to call the interpreter.
When numeric prefix arg is other than 0 or 4 do not SHOW."
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Run Godot: " (gdscript-godot--build-shell-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (gdscript-godot--build-shell-command) nil t)))
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
  (gdscript-run-command
   (concat (gdscript-godot--build-shell-command) " -e")))

(defun gdscript-godot-run-project ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ()
    (gdscript-run-command
     (gdscript-godot--build-shell-command))))

(defun gdscript-godot-run-project-debug ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ()
    (gdscript-run-command
     (concat (gdscript-godot--build-shell-command) " -d"))))

(defun gdscript-godot-run-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-run-command
   (concat (gdscript-godot--build-shell-command) " "
           (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))

(defun gdscript-godot-run-current-scene-debug ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-run-command
   (concat (gdscript-godot--build-shell-command) " -d "
           (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))

(defun gdscript-godot-edit-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-run-command
   (concat (gdscript-godot--build-shell-command) " -e "
           (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))

(defun gdscript-godot-run-current-script ()
  "Run the current script file in Godot Engine.

For this to work, the script must inherit either from
\"SceneTree\" or \"MainLoop\"."
  (interactive)
  (gdscript-run-command
   (concat (gdscript-godot--build-shell-command) " -s " (file-relative-name buffer-file-name))))

(provide 'gdscript-godot)
;;; gdscript-godot.el ends here
