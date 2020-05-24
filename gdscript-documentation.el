;;; gdscript-documentation.el --- Open documntation in Godot -*- lexical-binding: t; -*-
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
;;  Browse Godot API documentation in eww browser.
;;
;;; Code:

(require 'eww)

;;;###autoload
(defun gdscript-documentation-api ()
  "Open main page of Godot API in eww browser."
  (interactive)
  (eww-browse-url "https://docs.godotengine.org/en/stable/classes/index.html?#godot-api"))

(defun gdscript-documentation-open ()
  "Open documention for symbol at point in eww browser.
If a page is already open, it will switch to its buffer."
  (interactive)
  (let* ((symbol (downcase (thing-at-point 'symbol t)))
         (buffer
          (seq-find
           (lambda (current-buffer)
             (with-current-buffer current-buffer
               (when (derived-mode-p 'eww-mode)
                 (string-suffix-p symbol (plist-get eww-data :url) t)
                 ))) (buffer-list))))
    (if buffer (pop-to-buffer-same-window buffer)
      (eww-browse-url (format "https://docs.godotengine.org/en/stable/classes/class_%s.html#%s" symbol symbol) t))))

(defun gdscript-documentation-rename-eww-buffer ()
  "Rename eww buffer visiting Godot documentation.
It will rename eww buffer from generic name to name including page title."
  (when (derived-mode-p 'eww-mode)
    (let ((title (plist-get eww-data :title)))
      (when (string-match "Godot Engine" title)
        (rename-buffer (format "*eww - %s*" title) t)))))

(provide 'gdscript-documentation)
;;; gdscript-documentation.el ends here
