;;; gdscript-format.el --- GDScript formatting with gdformat -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Pawel Lampe

;; Author: Pawel Lampe <pawel.lampe@gmail.com>, Nathan Lovato <nathan@gdquest.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Feb 2020
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

;; Format a buffer containing GDScript code with the gdformat GDScript formatter.
;; This code is derived from gdtoolkit, see https://github.com/Scony/godot-gdscript-toolkit

;;; Code:

(defun gdscript-format--format-region (start end)
  "Format the region between START and END using `gdformat'."
  (save-excursion
    (shell-command-on-region
     start end
     (concat
      "echo "
      (shell-quote-argument (buffer-substring start end))
      "|"
      gdscript-gdformat-executable " -")
     (buffer-name) t)))

(defun gdscript-format-region()
  "Format the selected region using `gdformat'"
  (interactive)
  (gdscript-format--format-region
   (region-beginning) (region-end)))

(defun gdscript-format-buffer()
  "Format the entire current buffer using `gdformat'"
  (interactive)
  (let ((original-point (point))
        (original-window-pos (window-start)))
    (gdscript-format--format-region
     (point-min) (point-max))
    (goto-char original-point)
    (set-window-start (selected-window) original-window-pos)))

(provide 'gdscript-format)

;;; gdscript-format.el ends here
