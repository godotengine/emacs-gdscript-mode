;;; gdscript-menu.el --- Godot transient menus -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 GDQuest and contributors.

;; Author: John Charman <jchar@mailfence.com>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>

;; This file is not part of GNU Emacs.

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

;; Discoverable menus for common tasks in Godot game development.
;; - Call `gdscript-menu' for the main entrypoint.
;; - Call `gdscript-menu-godot' for launching the Godot engine.
;; - Call `gdscript-menu-gdscript' for GDScript.

;;; Code:

(require 'transient) ; Built-in

;; For gdscript-menu-godot commands
(require 'gdscript-godot)

;; For gdscript-menu-gdscript commands
(require 'gdscript-docs)
(require 'gdscript-format)

; To display project name all menus
(require 'gdscript-utils)


;; Main Menu
(transient-define-prefix gdscript-menu ()
  "Main menu for Godot."
  ["Godot Main Menu"
   :description (lambda ()
                  (format "Godot: %s" (gdscript-util--get-godot-project-name)))
   ("g" "Godot Engine" gdscript-menu-godot)
   ("s" "GDScript" gdscript-menu-gdscript)])


;; Godot Menu
(transient-define-prefix gdscript-menu-godot ()
  "Menu for launching the Godot executable."
  :history-key 'gdscript-menu-godot-history
  :incompatible '(("--debug" "--editor"))
  ["Godot Executable"
   :description (lambda ()
                  (format "Godot Engine: %s" (gdscript-util--get-godot-project-name)))
   [("<f5>" "project" gd//run-project)]
   [("<f6>" "current scene" gd//run-current-scene)]]
  [["Run Options"
    ("-d" "stdout debugger" "--debug")
    ("-e" "open in editor" "--editor")
    ("-h" "headless mode" "--headless")
    ("-r" "recovery mode" "--recovery-mode")]
   ["Debug Options"
    ("-c" "show collision shapes" "--debug-collisions")
    ("-p" "show path lines" "--debug-paths")
    ("-n" "show navigation polygons" "--debug-navigation")
    ("-a" "show navigation avoidance" "--debug-avoidance")]])

(transient-define-suffix gd//run-project (prefix)
  "Report the PREFIX-ARG, prefix's scope, and infix values."
  (interactive "P")
  (transient-save)
  (let ((args (transient-args transient-current-command)))
    (gdscript-comint--run
     `(,@args
       "--path" ,(gdscript-util--find-project-configuration-file)))))


(transient-define-suffix gd//run-current-scene (prefix)
  "Report the PREFIX-ARG, prefix's scope, and infix values."
  (interactive "P")
  (transient-save)
  (let ((args (transient-args transient-current-command)))
    (gdscript-comint--run
     `(,@args
       "--scene" ,(gdscript-godot--select-scene)))))


;; GDScript menu
(transient-define-prefix gdscript-menu-gdscript ()
  "Menu for GDScript files"
  ["GDScript"
   :description (lambda () (format "GDScript: %s"
                                   (gdscript-util--get-godot-project-name)))
   ["Docs"
    ("d" "browse" gdscript-docs-browse-api)]
   ["GDFormat"
    :description (lambda () (if (file-exists-p gdscript-gdformat-executable)
                                "GDFormat"
                                "GDFormat (not found)"))
    :inapt-if-not (lambda () (file-exists-p gdscript-gdformat-executable))
    ("f" "buffer" gdscript-format-buffer)
    ("F" "all" gdscript-format-all)]])


(provide 'gdscript-menu)
;; Local Variables:
;; read-symbol-shorthands: (("gd//" . "gdscript-menu--"))
;; End:
;;; gdscript-menu.el ends here
