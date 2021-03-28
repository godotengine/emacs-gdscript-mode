;;; gdscript-syntax.el --- Syntax highlighting for GDScript -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
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

;; Adds syntax highlighting and builds the syntax table for GDScript.

;;; Code:

(require 'cl-lib)
(require 'gdscript-keywords)

(defun gdscript-syntax-regex-maker (words)
  (regexp-opt words 'symbols))

;;; Font-lock and syntax
(eval-and-compile (defun gdscript-syntax--context-compiler-macro (form type &optional syntax-ppss)
                    (pcase type
                      (''comment
                       `(let ((ppss (or ,syntax-ppss
                                        (syntax-ppss))))
                          (and (nth 4 ppss)
                               (nth 8 ppss))))
                      (''string
                       `(let ((ppss (or ,syntax-ppss
                                        (syntax-ppss))))
                          (and (nth 3 ppss)
                               (nth 8 ppss))))
                      (''paren
                       `(nth 1
                             (or ,syntax-ppss
                                 (syntax-ppss))))
                      (_ form))))

;; Controls font-face mappings or colors to highlight groups of keywords
(defvar gdscript-font-lock `((,(rx (and "$" (one-or-more (or "/" (one-or-more word)))))
                              (0 font-lock-constant-face))
                             (,(rx (and (group "$") "\"" (zero-or-more nonl) "\""))
                              (1 font-lock-constant-face))
                             (,(gdscript-syntax-regex-maker gdscript-keywords)
                              1
                              font-lock-keyword-face)
                             (,(gdscript-syntax-regex-maker (append gdscript-built-in-constants
                                                                    gdscript-built-in-types gdscript-built-in-functions))
                              1
                              font-lock-builtin-face)
                             (,(gdscript-syntax-regex-maker gdscript-built-in-classes)
                              1
                              font-lock-type-face)
                             (,(rx symbol-start
                                   "func"
                                   (1+ space)
                                   (group (1+ (or word ?_))))
                              (1 font-lock-function-name-face))
                             (,(rx symbol-start
                                   (or "var" "const")
                                   (1+ space)
                                   (group (1+ (or word ?_))))
                              (1 font-lock-variable-name-face))
                             ;; Function call
                             (,(rx (group (1+ (or word ?_)))
                                   (0+ space)
                                   "(")
                              (1 font-lock-function-name-face))))

(defun gdscript-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro gdscript-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun gdscript-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(define-inline gdscript-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(define-inline gdscript-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eq (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))

(defconst gdscript-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "\"\"\"" "'''"))
    (0 (ignore (gdscript-syntax-stringify))))))

(defun gdscript-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun gdscript-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (cond ((or (nth 4 ppss)             ;Inside a comment
               (and string-start
                    ;; Inside of a string quoted with different triple quotes.
                    (not (eq (char-after string-start)
                             (char-after quote-starting-pos)))))
           ;; Do nothing.
           nil)
          ((nth 5 ppss)
           ;; The first quote is escaped, so it's not part of a triple quote!
           (goto-char (1+ quote-starting-pos)))
          ((null string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          (t
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defvar gdscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Gdscript files.")

(defvar gdscript-dotty-syntax-table
  (let ((table (make-syntax-table gdscript-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Gdscript files.
It makes underscores and dots word constituent chars.")

(provide 'gdscript-syntax)

;;; gdscript-syntax.el ends here
