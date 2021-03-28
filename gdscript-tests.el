;;; gdscript-tests.el --- tests for gdscript mode -*- lexical-binding: t; -*-
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
;; To run the tests in terminal:
;; > ./makem.sh test
;;
;;; Code:

(require 'ert)
(require 'gdscript-mode)

;; NOTE: `gdscript-tests--with-temp-buffer' and `gdscript-tests-look-at' are
;; originally from the Python package

(defmacro gdscript-tests--with-temp-buffer (contents &rest body)
  "Create a `gdscript-mode' enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ()
       (gdscript-mode)
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

(defun gdscript-tests-look-at (string &optional num restore-point)
  "Move point at beginning of STRING in the current buffer.
Optional argument NUM defaults to 1 and is an integer indicating
how many occurrences must be found, when positive the search is
done forwards, otherwise backwards.  When RESTORE-POINT is
non-nil the point is not moved but the position found is still
returned.  When searching forward and point is already looking at
STRING, it is skipped so the next STRING occurrence is selected."
  (let* ((num (or num 1))
         (starting-point (point))
         (string (regexp-quote string))
         (search-fn (if (> num 0) #'re-search-forward #'re-search-backward))
         (deinc-fn (if (> num 0) #'1- #'1+))
         (found-point))
    (prog2
        (catch 'exit
          (while (not (= num 0))
            (when (and (> num 0)
                       (looking-at string))
              ;; Moving forward and already looking at STRING, skip it.
              (forward-char (length (match-string-no-properties 0))))
            (and (not (funcall search-fn string nil t))
                 (throw 'exit t))
            (when (> num 0)
              ;; `re-search-forward' leaves point at the end of the
              ;; occurrence, move back so point is at the beginning
              ;; instead.
              (forward-char (- (length (match-string-no-properties 0)))))
            (setq
             num (funcall deinc-fn num)
             found-point (point))))
        found-point
      (and restore-point (goto-char starting-point)))))

(defun gdscript-tests-self-insert (char-or-str)
  "Call `self-insert-command' for chars in CHAR-OR-STR."
  (let ((chars
         (cond
          ((characterp char-or-str)
           (list char-or-str))
          ((stringp char-or-str)
           (string-to-list char-or-str))
          ((not
            (cl-remove-if #'characterp char-or-str))
           char-or-str)
          (t (error "CHAR-OR-STR must be a char, string, or list of char")))))
    (mapc
     (lambda (char)
       (let ((last-command-event char))
         (call-interactively 'self-insert-command)))
     chars)))

(ert-deftest gdscript-indent--comment-line ()
  "Test if current line is comment."
  (gdscript-tests--with-temp-buffer
   "
func f():
	# aaa
	bbb
"
   (gdscript-tests-look-at "aaa")
   (should (gdscript-info-current-line-comment-p))
   (gdscript-tests-look-at "bbb")
   (should (not (gdscript-info-current-line-comment-p)))))

(ert-deftest gdscript-indent--electric-colon-4 ()
  "Test indentation case where there is one more-indented previous open block."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if true or true:
		a = 5

		if true and true:
			a = 10

		b = 3

else
"
   (gdscript-tests-look-at "else")
   (goto-char (line-end-position))
   (gdscript-tests-self-insert ":")
   (gdscript-tests-look-at "else" -1)
   (should (= (current-indentation) 4))))

(ert-deftest gdscript-tests--indent-electric-colon-8 ()
  "Test indentation case where there is one more-indented previous open block, but it is comment."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if true or true:
		a = 5

		if true and true:
			a = 10

		# b = 3

else
"
   (gdscript-tests-look-at "else")
   (goto-char (line-end-position))
   (gdscript-tests-self-insert ":")
   (gdscript-tests-look-at "else" -1)
   (should (= (current-indentation) 8))))

(ert-deftest gdscript-tests--dedent-if-else ()
  "Test dedentation case where else is dedented from inner if to outer if."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if true:
		if true:
			a = 5
		else:
"
   (gdscript-tests-look-at "else")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--dedent-if-elif ()
  "Test dedentation case where elif is dedented from inner if to outer if."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if true:
		if true:
			a = 5
		elif true:
"
   (gdscript-tests-look-at "elif")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--dedent-elif-else ()
  "Test dedentation case where else is dedented from inner elif to outer if."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if true:
		if true:
			a = 5
		elif true:
			a = 6
		else:
"
   (gdscript-tests-look-at "else")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--indent-if-with-parens ()
  "Test indentation where if is followed by expression in parens."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if (
		true
	):
aaa = 5
"
   (gdscript-tests-look-at "aaa")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--indent-elif-with-parens ()
  "Test indentation where elif is followed by expression in parens."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if (
		true
	):
		aaa = 5
	elif(
		true
	):
aaa = 6
"
   (gdscript-tests-look-at "aaa = 6")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--indent-while-with-parens ()
  "Test indentation case where while is followed by expression in parens."
  (gdscript-tests--with-temp-buffer
   "
func f():
	while (
		true
	):
aaa = 5
"
   (gdscript-tests-look-at "aaa")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--indent-func-with-parens ()
  "Test indentation case where function params span multiple lines."
  (gdscript-tests--with-temp-buffer
   "
func f(
	i: int
):
pass
"
   (gdscript-tests-look-at "pass")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 4)))))

(ert-deftest gdscript-tests--indent-nested-ifs-with-parens ()
  "Test indentation where if is nested."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if (
		true
	):
		if (
			true
		):
aaa
"
   (gdscript-tests-look-at "aaa")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 12))
     (indent-for-tab-command)
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4))
     (indent-for-tab-command)
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 12)))))

(ert-deftest gdscript-tests--dedent-nested-ifs-with-parens ()
  "Test dedentation where if is nested."
  (gdscript-tests--with-temp-buffer
   "
func f():
	if (
		true
	):
		if (
			true
		):
			aaa
else:
"
   (gdscript-tests-look-at "else")
   (let ((last-command 'indent-for-tab-command)
         (this-command 'indent-for-tab-command))
     (should (= (current-indentation) 0))
     (indent-for-tab-command)
     (should (= (current-indentation) 8))
     (indent-for-tab-command)
     (should (= (current-indentation) 4))
     (indent-for-tab-command)
     (should (= (current-indentation) 8)))))
