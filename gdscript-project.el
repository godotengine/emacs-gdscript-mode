;;; gdscript-project.el --- Project -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
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

(defvar gdscript-project--script-list nil)

(defun gdscript-project--current-buffer-scene ()
  "Return the name of current scene.

If current buffer is not visiting scene file return nil."
  (when buffer-file-name
    (let ((scene-name (concat (gdscript-util--get-godot-project-file-path-relative buffer-file-name) ".tscn")))
      (when (file-exists-p scene-name) scene-name))))

(defun gdscript-project--select-scene ()
  "Find all scenes files and let user choose one."
  (let* ((rl (gdscript-util--find-project-configuration-file))
         (scene-list (mapcar (lambda (x) (file-relative-name x rl)) (directory-files-recursively rl ".*.tscn" t)))
         (prompt (format "Buffer %s is not scene file, select scene to run" (buffer-name))))
    (gdscript-util--read scene-list prompt)))

(defun gdscript-project--current-buffer-script ()
  "Return the name of current script.

If current buffer is not visiting script file return nil."
  (when buffer-file-name
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^extends SceneTree\\|^extends MainLoop" nil t)
        (file-relative-name buffer-file-name (gdscript-util--find-project-configuration-file))))))

(defun gdscript-project--select-script ()
  "Find all script files and let user choose one."
  (let ((hydra-open gdscript-hydra--open))
    (when hydra-open (gdscript-hydra--menu/nil))
    (unwind-protect
        (let* ((prompt (format "Buffer %s is not script file, select script to run" (buffer-name)))
               (script-name (gdscript-util--read gdscript-project--script-list prompt)))
          (gdscript-godot--run-script script-name))
      (when hydra-open (gdscript-hydra--menu/body)))))

(defun gdscript-project--ag-cleanup ()
  "Clean after ag search.

Try to leave Emacs as it was before ag search was launched."
  (remove-hook 'ag-search-finished-hook #'gdscript-project--ag-find-next-script)
  (let* ((ag-buffer (current-buffer))
         (ag-window (get-buffer-window ag-buffer))
         (prev-buffer (window-prev-buffers ag-window)))
    (if prev-buffer (kill-buffer ag-buffer)
      (delete-window ag-window)
      (kill-buffer ag-buffer))))

(defun gdscript-project--ag-find-next-script ()
  "Find next script file in ag buffer."
  (let ((pos (next-single-property-change (point) 'compilation-message))
        (comp-mes (get-text-property (point) 'compilation-message)))
    (when comp-mes
      (let ((file-name (caar (compilation--loc->file-struct (compilation--message->loc comp-mes)))))
        (push file-name gdscript-project--script-list)))
    (if pos (progn (goto-char pos)
                   (gdscript-project--ag-find-next-script))
      (gdscript-project--ag-cleanup)
      (with-current-buffer (window-buffer (selected-window))
        (gdscript-project--select-script)))))

(defun gdscript-project--get-all-scripts ()
  "Find all script files and let user choose one.

Since detection of script files require inspection of file contents,
this use ag for performance."
  (if (not (featurep 'ag))
      (error (format "Buffer %s is no script file. To see all available scripts in current project install package 'ag'." (buffer-name)))
    (ag-project-regexp "^extends SceneTree|^extends MainLoop")
    (setq gdscript-project--script-list nil)
    (add-hook 'ag-search-finished-hook #'gdscript-project--ag-find-next-script)))

(provide 'gdscript-project)

;;; gdscript-project.el ends here
