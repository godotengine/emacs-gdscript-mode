;;; gdscript-comint.el --- Support for comint mode -*- lexical-binding: t; -*-
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
;;  godot-mode for handling stdout and stderr from godot executable.
;;
;;  It support quick navigation from errors to file location.
;;
;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'gdscript-utils)

(defvar gdscript-comint--mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    (define-key map (kbd "C-a") 'comint-bol)
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map)
  "Basic mode map for `godot-mode'.")

(defun gdscript-comint--run (arguments)
  "Run godot in comint mode.

ARGUMENTS are command line arguments for godot executable.
When run it will kill existing process if one exists."
  (let ((buffer-name (gdscript-util--get-godot-buffer-name))
        (inhibit-read-only 1))

    (when (not (executable-find gdscript-godot-executable))
      (error "Error: Could not find %s on PATH.  Please customize the gdscript-godot-executable variable" gdscript-godot-executable))

    ;; start new godot
    (with-current-buffer (get-buffer-create buffer-name)
      (unless (derived-mode-p 'godot-mode)
        (godot-mode)
        (buffer-disable-undo))
      (erase-buffer)
      (comint-exec (current-buffer) buffer-name gdscript-godot-executable nil arguments))))

(define-derived-mode godot-mode comint-mode "godot"
  "Major mode for godot.

\\{gdscript-comint--mode-map}"
  (use-local-map gdscript-comint--mode-map)
  (add-hook 'godot-mode-hook 'gdscript-comint--initialize-for-comint-mode)
  (add-hook 'godot-mode-hook 'gdscript-comint--initialize-for-compilation-mode))

(defun gdscript-comint--initialize-for-comint-mode ()
  "Initialize buffer for comint mode support."
  (when (derived-mode-p 'comint-mode)
    (setq comint-process-echoes nil)
    (setq comint-prompt-regexp "debug> ")
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq ansi-color-for-comint-mode t)))

(defun gdscript-comint--initialize-for-compilation-mode ()
  "Initialize buffer for compilation mode support."
  (setq-local
   compilation-error-regexp-alist
   '(
     ("^   At: res://\\([[:word:]\/]+.gd\\):\\([[:digit:]]+\\)." 1 2 nil 2 1)
     ("^*Frame [[:digit:]]+ - res://\\([[:word:]\/]+.gd\\):\\([[:digit:]]+\\)." 1 2 nil 2 1)))
  (setq-local compilation-mode-font-lock-keywords nil)
  (compilation-setup t))

(provide 'gdscript-comint)
;;; gdscript-comint.el ends here
