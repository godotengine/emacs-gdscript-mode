;;; gdscript-rx.el --- Regex for GDScript -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Free Software Foundation, Inc.

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
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

;; Macro that defines named regular expressions to detect syntactic elements in
;; GDScript code.

;;; Code:

;; gdscript-rx is a copy of Emacs 27's rx module, to ensure compatibility with
;; Emacs 26
(if (version< emacs-version "27")
    (require 'emacs-rx))

;;; GDScript regex
(defmacro gdscript-rx (&rest regexps)
  "Gdscript mode specialized rx macro.
This variant of `rx' supports common Gdscript named REGEXPS."
  `(rx-let ((block-start       (seq (zero-or-more nonl)
                                    ":"
                                    (or (seq (zero-or-more " ") eol)
                                        (seq (zero-or-more " ") "#" (zero-or-more nonl) eol))))
            (dedenter          (seq symbol-start
                                    (or "elif" "else")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or "break" "continue" "pass" "return")
                                    symbol-end))
            (defun             (seq symbol-start
                                    (or "func" "class" "static func")
                                    symbol-end))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not simple-operator))
            ;; TODO: clean up operators that don't exist in GDScript
            (operator          (or "==" ">=" "is" "not"
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'")))))
     (rx ,@regexps)))

(provide 'gdscript-rx)

;;; gdscript-rx.el ends here
