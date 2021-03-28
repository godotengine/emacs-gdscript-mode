;;; gdscript-fill-paragraph.el --- GDScript fill paragraph functions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Jan 2020
;; Keywords: languages


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Code to fill paragraphs, fill strings and comments up to the fill-column.

;;; Code:

(require 'gdscript-customization)
(require 'gdscript-indent-and-nav)
(require 'gdscript-rx)

;; NOTE: this and the fill docstring function can be simplified. They're
;; originally from the Python package, which supports multiple docstrings fill
;; style.

(defun gdscript-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified."
  (interactive "P")
  (save-excursion
    (cond
     ;; Comments
     ((gdscript-syntax-context 'comment)
      (funcall gdscript-fill-comment-function justify))
     ;; Strings/Docstrings
     ((save-excursion (or (gdscript-syntax-context 'string)
                          (equal (string-to-syntax "|")
                                 (syntax-after (point)))))
      (funcall gdscript-fill-string-function justify))
     ;; Decorators
     ((equal (char-after (save-excursion
                           (gdscript-nav-beginning-of-statement))) ?@))
     ;; Parens
     ((or (gdscript-syntax-context 'paren)
          (looking-at (gdscript-rx open-paren))
          (save-excursion
            (skip-syntax-forward "^(" (line-end-position))
            (looking-at (gdscript-rx open-paren))))
      (funcall gdscript-fill-paren-function justify))
     (t t))))

(defun gdscript-fill-paragraph-fill-comment (&optional justify)
  "Comment fill function for `gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (fill-comment-paragraph justify))

(defun gdscript-fill-paragraph-fill-string (&optional justify)
  "String fill function for `gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (let* ((str-start-pos
          (set-marker
           (make-marker)
           (or (gdscript-syntax-context 'string)
               (and (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (point)))))
         (num-quotes (gdscript-syntax-count-quotes
                      (char-after str-start-pos) str-start-pos))
         (str-end-pos
          (save-excursion
            (goto-char (+ str-start-pos num-quotes))
            (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                (goto-char (point-max)))
            (point-marker)))
         (fill-paragraph-function))
    
    (save-restriction
      (narrow-to-region str-start-pos str-end-pos)
      (fill-paragraph justify)
      t)))

(defun gdscript-fill-paragraph-fill-paren (&optional justify)
  "Paren fill function for `gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (gdscript-syntax-context 'paren)
                          (goto-char (1- (point))))
                        (line-beginning-position))
                      (progn
                        (unless (gdscript-syntax-context 'paren)
                          (end-of-line)
                          (unless (gdscript-syntax-context 'paren)
                            (skip-syntax-backward "^)")))
                        (while (and (gdscript-syntax-context 'paren)
                                    (not (eobp)))
                          (goto-char (1+ (point))))
                        (point)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (gdscript-indent-line)
      (goto-char (line-end-position))))
  t)

(defun gdscript-fill-paragraph-do-auto-fill ()
  "Like `do-auto-fill', but bind `fill-indent-according-to-mode'."
  ;; See Bug#36056.
  (let ((fill-indent-according-to-mode t))
    (do-auto-fill)))

(provide 'gdscript-fill-paragraph)

;;; gdscript-fill-paragraph.el ends here
