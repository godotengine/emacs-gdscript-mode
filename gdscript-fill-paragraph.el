;;; gdscript-fill-paragraph.el --- GDScript fill paragraph functions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Free Software Foundation, Inc.

;; Author: Nathan Lovato <nathan@gdquest.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
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
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Code to fill paragraphs, fill strings and comments up to the fill-column.

;;; Code:

(require 'gdscript-customization)
(require 'gdscript-indent-and-nav)
(require 'gdscript-rx)

;; NOTE: this and the fill docstring function can be simplified. They're
;; originally from the Python package, which supports multiple docstrings fill
;; style.
(setq-local delimiters-style (cons nil 1))

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
                           (gdscript-nav-beginning-of-statement))) ?@)
      (funcall gdscript-fill-decorator-function justify))
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
         (multi-line-p
          ;; Docstring styles may vary for oneliners and multi-liners.
          (> (count-matches "\n" str-start-pos str-end-pos) 0))
         (fill-paragraph-function))
    (save-restriction
      (narrow-to-region str-start-pos str-end-pos)
      (fill-paragraph justify))
    (save-excursion
      (when (and (gdscript-info-docstring-p) gdscript-fill-triple-string-style)
        ;; Add the number of newlines indicated by the selected style
        ;; at the start of the docstring.
        (goto-char (+ str-start-pos num-quotes))
        (delete-region (point) (progn
                                 (skip-syntax-forward "> ")
                                 (point)))
        (and (car delimiters-style)
             (or (newline (car delimiters-style)) t)
             ;; Indent only if a newline is added.
             (indent-according-to-mode))
        ;; Add the number of newlines indicated by the selected style
        ;; at the end of the docstring.
        (goto-char (if (not (= str-end-pos (point-max)))
                       (- str-end-pos num-quotes)
                     str-end-pos))
        (delete-region (point) (progn
                                 (skip-syntax-backward "> ")
                                 (point)))
        (and (cdr delimiters-style)
             ;; Add newlines only if string ends.
             (not (= str-end-pos (point-max)))
             (or (newline (cdr delimiters-style)) t)
             ;; Again indent only if a newline is added.
             (indent-according-to-mode))))) t)

(defun gdscript-fill-paragraph-fill-paren (&optional justify)
  "Paren fill function for `gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (gdscript-syntax-context 'paren)
                          (goto-char (1- (point))))
                        (line-beginning-position))
                      (progn
                        (when (not (gdscript-syntax-context 'paren))
                          (end-of-line)
                          (when (not (gdscript-syntax-context 'paren))
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
