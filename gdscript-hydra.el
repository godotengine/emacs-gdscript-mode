;;; gdscript-hydra.el --- Hydra for launching Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
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
;;  Hydra for launching Godot. It offers options to launch Godot in debug mode or open editor or run the script.
;;
;;  When running in debug mode it offers two additional flags to pass to Godot:
;;   --debug-collisions
;;   --debug-navigation
;;
;;; Code:

(require 'hydra nil t)
(require 'gdscript-godot)

;;;###autoload
(defvar gdscript-hydra--debug nil)
(defvar gdscript-hydra--editor nil)
(defvar gdscript-hydra--debug-collisions nil)
(defvar gdscript-hydra--debug-navigation nil)

(defun gdscript-hydra-show ()
  "Show gdcript hydra."
  (interactive)
  (when (not (featurep 'hydra))
    (error "gdscript-hydra.el: No `hydra.el' available. To execute `gdscript-hydra-show' command you need to install hydra.el."))
  (gdscript-hydra--menu/body))

(defun gdscript-hydra--selected (selected)
  "Visual representation for (non)selected checkboxes."
  (if selected "x" " "))

(defun gdscript-hydra--dispatch (run-default run-debug run-editor)
  "Run Godot with selected flag.

RUN-DEFAULT is a function to call when neither debug or scene flag is selected in hydra.
RUN-DEBUG is a function to call when debug flag is selected in hydra.
RUN-EDITOR is a function to call when editor flag is selected in hydra.
"
  (cond
   ((and (not gdscript-hydra--debug)
         (not gdscript-hydra--editor)) (funcall run-default))
   (gdscript-hydra--debug (funcall run-debug))
   (gdscript-hydra--editor (funcall run-editor))))

(defun gdscript-hydra--run (project-or-scene)
  "Dispatcher from hydra heads to gdscript-godot-* commands.

It is setting variable `gdscript-godot--debug-options-hydra' based on hydra checkboxes."
  (setq gdscript-godot--debug-options-hydra
        (concat
         (when gdscript-hydra--debug-collisions "--debug-collisions ")
         (when gdscript-hydra--debug-navigation "--debug-navigation ")))

  (pcase project-or-scene
    (:project (gdscript-hydra--dispatch 'gdscript-godot-run-project
                                        'gdscript-godot-run-project-debug
                                        'gdscript-godot-open-project-in-editor))
    (:scene (gdscript-hydra--dispatch 'gdscript-godot-run-current-scene
                                      'gdscript-godot-run-current-scene-debug
                                      'gdscript-godot-edit-current-scene))
    (:script (gdscript-godot-run-current-script))))

(defun gdscript-hydra--open-godot-buffer ()
  "Find buffer named *godot* and if it exists open it in other window."
  (let ((godot-buffer (seq-find
                       (lambda (current-buffer)
                         (with-current-buffer current-buffer
                           (equal (buffer-name) "*godot*"))) (buffer-list))))
    (when godot-buffer (switch-to-buffer-other-window godot-buffer))))

(ignore-errors
  ;; Don't signal an error when hydra.el is not present
  (defhydra gdscript-hydra--menu (:hint none)
    "
_d_ (?d?) Debug   _p_ run current project  _t_ run current script  _q_ quit
_e_ (?e?) Editor  _s_ run current scene    _g_ switch to *godot*

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
    ("c" (setq gdscript-hydra--debug-collisions (not gdscript-hydra--debug-collisions)
               gdscript-hydra--debug t
               gdscript-hydra--editor nil) (gdscript-hydra--selected gdscript-hydra--debug-collisions))
    ("n" (setq gdscript-hydra--debug-navigation (not gdscript-hydra--debug-navigation)
               gdscript-hydra--debug t
               gdscript-hydra--editor nil) (gdscript-hydra--selected gdscript-hydra--debug-navigation))
    ("g" (gdscript-hydra--open-godot-buffer) :color blue)
    ("q" nil)))

(provide 'gdscript-hydra)

;;; gdscript-hydra.el ends here
