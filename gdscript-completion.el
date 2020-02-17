;;; gdscript-completion.el --- Autocompletion for GDScript -*- lexical-binding: t -*-

;; Copyright (C) 2020 GDQuest, Free Software Foundation, Inc.

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Feb 2020
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

;; Provides keywords for 'completion-at-point'.

;;; Code:

(require 'gdscript-syntax)

(defvar-local gdscript-completion--all-keywords
  (eval-when-compile (append gdscript-keywords gdscript-built-in-classes
                             gdscript-built-in-constants gdscript-built-in-functions
                             gdscript-built-in-types)))

(defun gdscript-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end (cdr bounds)))
    (list start end gdscript-completion--all-keywords
          . nil)))

(provide 'gdscript-completion)

;;; gdscript-completion.el ends here
