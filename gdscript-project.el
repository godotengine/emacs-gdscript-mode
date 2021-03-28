;;; gdscript-project.el --- Project -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: June 2020
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
;;  Find all scenes/scripts functions.
;;
;;; Code:

(require 'gdscript-utils)

(defun gdscript-project--current-buffer-scene ()
  "Return the name of current scene.

If current buffer is not visiting scene file return nil."
  (when buffer-file-name
    (let ((scene-name (concat
                       (gdscript-util--find-project-configuration-file)
                       (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))
      (when (file-exists-p scene-name) scene-name))))

(defun gdscript-project--select-scene ()
  "Find all scenes files and let user choose one. Return `nil' if user cancels selection."
  (message "selecting scene")
  (let* ((rl (gdscript-util--find-project-configuration-file))
         (scene-list (mapcar (lambda (x) (file-relative-name x rl)) (directory-files-recursively rl ".*.tscn" t)))
         (prompt (format "Select scene to run" (buffer-name)))
         (selected-scene (gdscript-util--read scene-list prompt)))
    selected-scene))

(defun gdscript-project--current-buffer-script ()
  "Return the name of current script.

If current buffer is not visiting script file return nil."
  (when buffer-file-name
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^extends SceneTree\\|^extends MainLoop" nil t)
        (file-relative-name buffer-file-name (gdscript-util--find-project-configuration-file))))))

(defun gdscript-project--select-script (script-list)
  "Find all script files and let user choose one."
  (let ((hydra-open gdscript-hydra--open))
    (when hydra-open (gdscript-hydra--menu/nil))
    (unwind-protect
        (let* ((prompt (format "Buffer %s is not script file, select script to run" (buffer-name)))
               (script-name (gdscript-util--read script-list prompt)))
          (when script-name
            (string-match "^\\(.*.gd\\):" script-name)
            (gdscript-godot--run-script (match-string 1 script-name))))
      (when hydra-open (gdscript-hydra--menu/body)))))

(defun gdscript-project--get-all-scripts ()
  "Find all script files and let user choose one.

Since detection of script files require inspection of file contents,
this use ag for performance."
  (if (executable-find "ag")
      (let ((default-directory (vc-git-root default-directory)))
        (with-temp-buffer
          (call-process "ag" nil (current-buffer) nil "--vimgrep" "-s" "^extends SceneTree|^extends MainLoop")
          (let ((available-standalone-scripts (split-string (buffer-string) "\n" t)))
            (if (null available-standalone-scripts)
                (message "No standalone script found. Look at https://docs.godotengine.org/en/stable/getting_started/editor/command_line_tutorial.html#running-a-script for details.")
              (gdscript-project--select-script available-standalone-scripts)))))
    (error (format "Buffer %s is no script file. To see all available scripts install 'ag' executable." (buffer-name)))))

(provide 'gdscript-project)

;;; gdscript-project.el ends here
