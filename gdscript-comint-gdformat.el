;;; gdscript-comint-gdformat.el --- gdformat mode based on comint mode -*- lexical-binding: t; -*-
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
;;  gdformat-mode for handling stdout and stderr from gdformat executable.
;;
;;  It supports quick navigation from errors to file location.
;;
;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'gdscript-customization)
(require 'gdscript-utils)

(defvar gdscript-comint-gdformat--mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map)
  "Basic mode map for `gdformat-mode'.")

(defun gdscript-comint-gdformat--sentinel (process event)
  "Display result of formatting if gdformat PROCESS exited abnormal EVENT."
  (when (string-match "exited abnormally" event)
    (pop-to-buffer (process-buffer process))))

(defun gdscript-comint-gdformat--run (arguments)
  "Run gdformat in comint mode.

ARGUMENTS are command line arguments for gdformat executable.
When run it will kill existing process if one exists."
  (let ((buffer-name (gdscript-util--get-gdformat-buffer-name))
        (inhibit-read-only t))

    (when (not (executable-find gdscript-gdformat-executable))
      (error "Error: Could not find %s on PATH.  Please customize the gdscript-gdformat-executable variable" gdscript-gdformat-executable))

    (with-current-buffer (get-buffer-create buffer-name)
      (unless (derived-mode-p 'gdformat-mode)
        (gdformat-mode)
        (buffer-disable-undo))
      (erase-buffer)
      (let* ((line-length (list (format "--line-length=%s" gdscript-gdformat-line-length)))
             (buffer (comint-exec (current-buffer) buffer-name gdscript-gdformat-executable nil (append line-length arguments))))
        (set-process-sentinel (get-buffer-process buffer) 'gdscript-comint-gdformat--sentinel)
        buffer))))

(define-derived-mode gdformat-mode comint-mode "gdformat"
  "Major mode for gdformat.

\\{gdscript-comint-gdformat--mode-map}"
  (use-local-map gdscript-comint-gdformat--mode-map)
  (add-hook 'gdformat-mode-hook 'gdscript-comint-gdformat--initialize-for-comint-mode)
  (add-hook 'gdformat-mode-hook 'gdscript-comint-gdformat--initialize-for-compilation-mode))

(defun gdscript-comint-gdformat--initialize-for-comint-mode ()
  "Initialize buffer for comint mode support."
  (when (derived-mode-p 'comint-mode)
    (setq comint-process-echoes nil)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq ansi-color-for-comint-mode t)))

(defun gdscript-comint-gdformat--failed-file-name()
  "Find corresponding buffer name for error message: 'at line x col y'."
  (save-excursion
    (save-match-data
      (re-search-backward "exception during formatting of \\(.*\\)")
      (match-string-no-properties 1))))

(defun gdscript-comint-gdformat--initialize-for-compilation-mode ()
  "Initialize buffer for compilation mode support."
  (setq-local
   compilation-error-regexp-alist
   '(("at line \\([[:digit:]]+\\) col \\([[:digit:]]+\\)" gdscript-comint-gdformat--failed-file-name 1 2 nil nil)
     ("exception during formatting of \\(.*\\)$" 1 nil nil nil 1)
     ("reformatted \\(.*\\)$" 1 nil nil nil 1)))
  (setq-local compilation-mode-font-lock-keywords nil)
  (compilation-setup t))

(provide 'gdscript-comint-gdformat)
;;; gdscript-comint-gdformat.el ends here
