;;; gdscript-godot.el --- Open and run projects in Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Franco Eusébio Garcia <francoegarcia@outlook.com>, Nathan Lovato <nathan@gdquest.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
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

(require 'gdscript-comint)
(require 'gdscript-customization)
(require 'gdscript-debug)
(require 'gdscript-history)
(require 'gdscript-project)
(require 'gdscript-utils)

;;;###autoload
(defvar gdscript-godot--debug-options-hydra :not-list)

(defvar gdscript-godot--debug-selected-option 1)

(defvar gdscript-godot--debug-options-alist
  '((1 . ())
    (2 . ("--debug-collisions"))
    (3 . ("--debug-navigation"))
    (4 . ("--debug-collisions" "--debug-navigation"))))

(defmacro gdscript-godot--debug-options-handler (debug-options &rest body)
  "Execute the forms in BODY with DEBUG-OPTIONS being set.

DEBUG-OPTIONS will be set either by Hydra, or by prefix argument selection."
  (declare (indent 1) (debug t))
  `(let* ((debug-option-index
           (if current-prefix-arg
               (gdscript-godot--change-debug-options)
             gdscript-godot--debug-selected-option))
          (prefix-options (cdr (assoc debug-option-index gdscript-godot--debug-options-alist)))
          (use-hydra-options (listp gdscript-godot--debug-options-hydra)) ;; gdscript-godot--debug-options-hydra is a list when run from hydra
          (,debug-options (if use-hydra-options gdscript-godot--debug-options-hydra prefix-options)))
     ,@body))

(defun gdscript-godot--run-command (&rest arguments)
  "Run a Godot process with ARGUMENTS.

The output of the process will be provided in a buffer named
`*godot - <project-name>*'."
  (let ((args (gdscript-util--flatten arguments)))
    (gdscript-history--add-to-history args)
    (when (and gdscript-debug--breakpoints (not (member "-e" arguments)))
       ;; Start debugger server if it is not running already
      (unless (get-process (gdscript-debug-process-name (gdscript-util--find-project-configuration-file)))
        (gdscript-debug-make-server))
      (push (mapconcat (lambda (breakpoint)
                         (let ((file (gdscript-breakpoint->file breakpoint))
                               (line (gdscript-breakpoint->line breakpoint)))
                           (format "%s:%s" file line))) gdscript-debug--breakpoints ",") args)
      (push "--breakpoints" args)
      (push (format "127.0.0.1:%s" gdscript-debug-port) args)
      (push "--remote-debug" args))
    (gdscript-comint--run (append (gdscript-godot--build-shell-command) args))
    (setq gdscript-godot--debug-options-hydra :not-list)))

(defun gdscript-godot--build-shell-command (&optional path)
  "Build a shell command to with the Godot executable.

If PATH is not provided, try to find it using the current
file's directory as starting point."
  (let ((project-path (or path (gdscript-util--find-project-configuration-file))))
    (list "--path" project-path)))

(defun gdscript-godot-open-project-in-editor ()
  "Run Godot Engine Editor."
  (interactive)
  (gdscript-godot--run-command "-e"))

(defun gdscript-godot-run-project ()
  "Run the current project in Godot Engine."
  (interactive)
  (gdscript-godot--run-command))

(defun gdscript-godot-run-project-debug ()
  "Run the current project in Godot Engine.

When run with prefix argument, it offers extra debug options to choose from."
  (interactive)
  (gdscript-godot--debug-options-handler debug-options
    (gdscript-godot--run-command "-d" debug-options)))

(defun gdscript-godot-run-current-scene ()
  "Run the current script file in Godot Engine. Use the universal prefix (C-u) to force a scene select."
  (interactive)
  (let ((scene (gdscript-godot--select-scene current-prefix-arg)))
    (when scene
      (gdscript-godot--run-command scene))))

(defun gdscript-godot-run-current-scene-debug ()
  "Run the current script file in Godot Engine.

When run with prefix argument, it offers extra debug options to choose from."
  (interactive)
  (let ((scene (gdscript-godot--select-scene current-prefix-arg)))
    (when scene
      (gdscript-godot--debug-options-handler debug-options
        (gdscript-godot--run-command "-d" debug-options scene)))))

(defun gdscript-godot-edit-current-scene ()
  "Run the current script file in Godot Engine."
  (interactive)
  (let ((scene (gdscript-godot--select-scene current-prefix-arg)))
    (when scene
      (gdscript-godot--run-command "-e" scene))))

(defun gdscript-godot--select-scene (&optional select-scene)
  "Select scene to run"
  (if select-scene
      (gdscript-project--select-scene)
    (let ((scene-name (gdscript-project--current-buffer-scene)))
      (if scene-name scene-name
        (gdscript-project--select-scene)))))

(defun gdscript-godot-run-current-script ()
  "Run the current script file in Godot Engine.

For this to work, the script must inherit either from
\"SceneTree\" or \"MainLoop\"."
  (interactive)
  (let ((script-name (gdscript-project--current-buffer-script)))
    (if script-name
        (gdscript-godot--run-script script-name)
      (gdscript-project--get-all-scripts))))

(defun gdscript-godot--run-script (script-name)
  "Run SCRIPT-NAME script in Godot Engine."
  (gdscript-godot--run-command "-s" script-name))

(defun gdscript-godot--debug-options-to-string (index)
  "Return debug options from `gdscript-godot--debug-options-alist' for given INDEX as a string."
  (mapconcat 'identity (cdr (assoc index gdscript-godot--debug-options-alist)) " "))

(defun gdscript-godot--debug-options-collection ()
  "Output a list of debug options to choose from by *-read function."
  (list
   (format "1) [%s] <no options>" (if (eq gdscript-godot--debug-selected-option 1) "X" " "))
   (format "2) [%s] %s" (if (eq gdscript-godot--debug-selected-option 2) "X" " ") (gdscript-godot--debug-options-to-string 2))
   (format "3) [%s] %s" (if (eq gdscript-godot--debug-selected-option 3) "X" " ") (gdscript-godot--debug-options-to-string 3))
   (format "4) [%s] %s" (if (eq gdscript-godot--debug-selected-option 4) "X" " ") (gdscript-godot--debug-options-to-string 4))))

(defun gdscript-godot--change-debug-options ()
  "Read debug option and parse it as a number.

Once read it is saved in `gdscript-godot--debug-selected-option'
variable for later use."
  (let* ((option (gdscript-util--read (gdscript-godot--debug-options-collection)))
         (index (string-to-number option)))
    (setq gdscript-godot--debug-selected-option index)))

(provide 'gdscript-godot)
;;; gdscript-godot.el ends here
