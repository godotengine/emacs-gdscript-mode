;;; gdscript-hydra.el --- Hydra for launching Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: May 2020
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
;;  Hydra for launching Godot.  It offers options to launch Godot in debug mode or open editor or run the script.
;;
;;  When running in debug mode it offers two additional flags to pass to Godot:
;;   --debug-collisions
;;   --debug-navigation
;;
;;; Code:

(require 'hydra nil t)
(require 'gdscript-format)
(require 'gdscript-godot)
(require 'gdscript-history)
(require 'gdscript-utils)

;;;###autoload
(defvar gdscript-hydra--open nil)
(defvar gdscript-hydra--debug nil)
(defvar gdscript-hydra--editor nil)
(defvar gdscript-hydra--debug-collisions nil)
(defvar gdscript-hydra--debug-navigation nil)
(defvar gdscript-hydra--hydra nil)

(defun gdscript-hydra-show ()
  "Show gdcript hydra."
  (interactive)
  (gdscript-util--with-available-hydra
   (unless gdscript-hydra--hydra
     (setq gdscript-hydra--hydra (gdscript-hydra--create)))
   (funcall gdscript-hydra--hydra)))

(defun gdscript-hydra--selected (selected)
  "Visual representation for (non)SELECTED checkboxes."
  (if selected "x" " "))

(defun gdscript-hydra--dispatch (run-default run-debug run-editor)
  "Run Godot with selected flag.

RUN-DEFAULT is a function to call when neither debug or scene flag
is selected in hydra.
RUN-DEBUG is a function to call when debug flag is selected in hydra.
RUN-EDITOR is a function to call when editor flag is selected in hydra."
  (cond
   ((and (not gdscript-hydra--debug)
         (not gdscript-hydra--editor)) (funcall run-default))
   (gdscript-hydra--debug (funcall run-debug))
   (gdscript-hydra--editor (funcall run-editor))))

(defun gdscript-hydra--run (project-or-scene)
  "Dispatcher from hydra heads to gdscript-godot-* commands.

PROJECT-OR-SCENE is symbol representing scene, project or script.
It is setting variable `gdscript-godot--debug-options-hydra' based
on hydra checkboxes."
  (setq gdscript-godot--debug-options-hydra
        (remove nil
                (list
                 (when gdscript-hydra--debug-collisions "--debug-collisions")
                 (when gdscript-hydra--debug-navigation "--debug-navigation"))))

  (pcase project-or-scene
    (:project (gdscript-hydra--dispatch 'gdscript-godot-run-project
                                        'gdscript-godot-run-project-debug
                                        'gdscript-godot-open-project-in-editor))
    (:scene (gdscript-hydra--dispatch 'gdscript-godot-run-current-scene
                                      'gdscript-godot-run-current-scene-debug
                                      'gdscript-godot-edit-current-scene))
    (:script (gdscript-godot-run-current-script))))

(defun gdscript-hydra--open-godot-buffer ()
  "Find buffer named *godot - <project-name>* and if it exists open it in other window."
  (let* ((current-name (buffer-name (current-buffer)))
         (godot-buffer-name (gdscript-util--get-godot-buffer-name)))
    (unless (string= godot-buffer-name current-name)
      (let ((godot-buffer (seq-find
                           (lambda (current-buffer)
                             (with-current-buffer current-buffer
                               (equal (buffer-name) godot-buffer-name))) (buffer-list))))
        (when godot-buffer (switch-to-buffer-other-window godot-buffer))))))

(defun gdscript-hydra--run-last ()
  "Run last command from history."
  (gdscript-godot--run-command (gdscript-history--last-command)))

(defun gdscript-hydra--select-from-history ()
  "Choose command to run from history of commands."
  (gdscript-godot--run-command (gdscript-history--select-from-history)))

(defun gdscript-hydra--create ()
  (defhydra gdscript-hydra--menu (:hint none
                                        :body-pre (setq gdscript-hydra--open t)
                                        :before-exit (setq gdscript-hydra--open nil))
    "
_d_ (?d?) Debug   _p_ run project  _t_ run script  _h_ run from history   _a_ format all     _q_ quit
_e_ (?e?) Editor  _s_ run scene    _r_ run last    _g_ switch to *godot*  _b_ format buffer

_c_ [?c?] Visible collisions shapes
_n_ [?n?] Visible navigation
"
    ("d" (progn
           (setq gdscript-hydra--debug (not gdscript-hydra--debug)
                 gdscript-hydra--editor nil)
           (unless gdscript-hydra--debug
             (setq
              gdscript-hydra--debug-collisions nil
              gdscript-hydra--debug-navigation nil))) (gdscript-hydra--selected gdscript-hydra--debug))
    ("e" (setq gdscript-hydra--editor (not gdscript-hydra--editor)
               gdscript-hydra--debug nil
               gdscript-hydra--debug-collisions nil
               gdscript-hydra--debug-navigation nil) (gdscript-hydra--selected gdscript-hydra--editor))
    ("p" (gdscript-hydra--run :project))
    ("s" (gdscript-hydra--run :scene))
    ("t" (gdscript-hydra--run :script))
    ("r" (gdscript-hydra--run-last))
    ("h" (gdscript-hydra--select-from-history))
    ("c" (setq gdscript-hydra--debug-collisions (not gdscript-hydra--debug-collisions)
               gdscript-hydra--debug t
               gdscript-hydra--editor nil) (gdscript-hydra--selected gdscript-hydra--debug-collisions))
    ("n" (setq gdscript-hydra--debug-navigation (not gdscript-hydra--debug-navigation)
               gdscript-hydra--debug t
               gdscript-hydra--editor nil) (gdscript-hydra--selected gdscript-hydra--debug-navigation))
    ("g" (gdscript-hydra--open-godot-buffer) :color blue)
    ("a" (gdscript-format-all))
    ("b" (gdscript-format-buffer))
    ("q" nil)))

(provide 'gdscript-hydra)

;;; gdscript-hydra.el ends here
