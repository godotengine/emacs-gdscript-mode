;;; gdscript-customization.el --- Customizable variables for the GDScript language support  -*- lexical-binding: t -*-

;; Copyright (C) 2020 GDQuest

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
;; see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Contains all the defcustom for gdscript-mode

;;; Code:

(defgroup gdscript nil
  "GDScript language support for Emacs."
  :group 'languages
  :version "26"
  :link '(emacs-commentary-link "gdscript"))

(defcustom gdscript-use-type-hints t
  "If t, inserted snippets contain type hints."
  :group 'gdscript
  :type 'boolean)

;; gdscript-indent
(defcustom gdscript-use-tab-indents t "Use tabs (t) or spaces (nil)."
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-tab-width 4 "Indentation width."
  :type 'integer
  :group 'gdscript)

(defcustom gdscript-indent-offset 4 "Default indentation offset for Gdscript."
  :group 'gdscript
  :type 'integer
  :safe 'integerp)

(defcustom gdscript-indent-guess-indent-offset
  t "If t, tells GDScript mode to guess `gdscript-indent-offset' value."
  :type 'boolean
  :group 'gdscript
  :safe 'booleanp)

(defcustom gdscript-indent-guess-indent-offset-verbose
  t "If t, emit a warning when guessing indentation fails."
  :version "25.1"
  :type 'boolean
  :group 'gdscript
  :safe 'booleanp)

(defcustom gdscript-indent-trigger-commands '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `gdscript-indent-line' call."
  :type '(repeat symbol):group'gdscript)

;; gdscript-fill-paragraph.el
(defcustom gdscript-fill-comment-function 'gdscript-fill-paragraph-fill-comment
  "Function to fill comments.
This is the function used by `gdscript-fill-paragraph' to
fill comments."
  :type 'symbol
  :group 'gdscript)

(defcustom gdscript-fill-string-function 'gdscript-fill-paragraph-fill-string
  "Function to fill strings.
This is the function used by `gdscript-fill-paragraph' to
fill strings."
  :type 'symbol
  :group 'gdscript)

(defcustom gdscript-fill-paren-function 'gdscript-fill-paragraph-fill-paren
  "Function to fill parens.
This is the function used by `gdscript-fill-paragraph' to
fill parens."
  :type 'symbol
  :group 'gdscript)

(defcustom gdscript-indent-def-block-scale 2
  "Multiplier applied to indentation inside multi-line def blocks."
  :version "26.1"
  :type 'integer
  :safe 'natnump)

(defcustom gdscript-godot-executable "godot"
  "The path to the Godot executable.
By default, it assumes that the executable is in the system's
PATH."
  :type 'string
  :group 'gdscript)

(defcustom gdscript-godot-shell-buffer-name "*Godot*"
  "Default buffer name for Godot running process."
  :type 'string
  :group 'gdscript
  :safe 'stringp)

(provide 'gdscript-customization)

;;; gdscript-customization.el ends here
