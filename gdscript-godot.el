;;; gdscript-godot.el --- Functions to run Godot Engine in gdscript-mode -*- lexical-binding: t; -*-

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

;; Functions to run the Godot Engine within gdscript-mode.

;;; Code:

(require 'gdscript-utils)

;;;###autoload
(defun gdscript-godot-run-command (cmd &optional show)
  "Run a Godot process.

If SHOW, the output of the process will be provided in a buffer
named after `gdscript-godot-shell-buffer-name'."
  (interactive)
  (start-process-shell-command "Godot Process"
                               (if show
                                   gdscript-godot-shell-buffer-name
                                 nil)
                               cmd))

(defun gdscript-godot-run-godot-editor ()
  "Run Godot Engine Editor."
  (interactive)
  (gdscript-godot-run-command
   (concat (gdscript-godot--build-shell-command) " -e")))

(defun gdscript-godot-run-project ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ((project-path (gdscript-util--find-project-configuration-path)))
    (gdscript-godot-run-command
     (gdscript-godot--build-shell-command))))

(defun gdscript-godot-run-project-debug-mode ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ((project-path (gdscript-util--find-project-configuration-path)))
    (gdscript-godot-run-command
     (concat (gdscript-godot--build-shell-command) " -d")
     t)))

(defun gdscript-godot-run-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-godot-run-command
   (concat (gdscript-godot--build-shell-command) " "
           (gdscript-godot--get-file-relative-path-to-project buffer-file-name) ".tscn")))

(defun gdscript-godot-run-current-scene-debug-mode ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-godot-run-command
   (concat (gdscript-godot--build-shell-command) " -d "
           (gdscript-godot--get-file-relative-path-to-project buffer-file-name) ".tscn")
   t))

(defun gdscript-godot-edit-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (gdscript-godot-run-command
   (concat (gdscript-godot--build-shell-command) " -e "
           (gdscript-godot--get-file-relative-path-to-project buffer-file-name) ".tscn")))

(defun gdscript-godot-run-current-script ()
  "Run the current script file in Godot Engine.

For this to work, the script must inherit either from
\"SceneTree\" or \"MainLoop\"."
  (interactive)
  (gdscript-godot-run-command
   (concat (gdscript-godot--build-shell-command) " -s " (file-relative-name buffer-file-name))
   t))

(defun gdscript-godot--build-shell-command (&optional path)
  "Build base shell command to run Godot Engine with the project's base PATH.

If PATH is not provided, try to find it using the current file's
directory as starting point."
  (let* ((project-path (or path (gdscript-util--find-project-configuration-path))))
    (concat gdscript-godot-executable " --path " project-path)))

(provide 'gdscript-godot)

;;; gdscript-godot.el ends here
