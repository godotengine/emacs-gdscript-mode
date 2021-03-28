;;; gdscript-customization.el --- Customizable variables for the GDScript language support  -*- lexical-binding: t -*-

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

;; Contains all the defcustom for gdscript-mode

;;; Code:

(defgroup gdscript nil
  "GDScript language support for Emacs."
  :group 'languages
  :version "26"
  :link '(emacs-commentary-link "gdscript"))

(defcustom gdscript-use-type-hints t
  "If t, inserted snippets contain type hints."
  :type 'boolean
    :group 'gdscript)

;; gdscript-indent
(defcustom gdscript-use-tab-indents t "Use tabs (t) or spaces (nil)."
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-tab-width 4 "Indentation width."
  :type 'integer
  :group 'gdscript)

(defcustom gdscript-indent-offset 4 "Default indentation offset for Gdscript."
  :type 'integer
  :safe 'integerp
  :group 'gdscript)

(defcustom gdscript-indent-trigger-commands '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `gdscript-indent-line' call."
  :type '(repeat symbol)
  :group'gdscript)

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
  :safe 'natnump
  :group 'gdscript)

(defcustom gdscript-indent-line-continuation-scale 1
  "Multiplier applied to indentation of line continuation in
general (inside parentheses and after backslash)."
  :type 'integer
  :options '(1 2)
  :group 'gdscript)

(defcustom gdscript-godot-executable "godot"
  "The godot executable which is either a full path such as '~/bin/godot2.2'
or the name of an executable on the system PATH (usually 'godot')"
  :type 'string
  :group 'gdscript)

(defcustom gdscript-gdformat-executable "gdformat"
  "The path to the gdformat executable.
By default, it assumes that the executable is in the system's
PATH."
  :type 'string
  :group 'gdscript)

(defcustom gdscript-gdformat-line-length 100
  "How many characters per line to allow when formatting gdscript by gdformat."
  :type 'integer
  :group 'gdscript)

(defcustom gdscript-gdformat-save-and-format nil
  "If t, save all modified buffers and format them with gdformat.
It happens anytime Godot executable is run.  Formatting runs on background,
so it is not slowing down Godot execution."
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-docs-force-online-lookup nil
  "If true, calling commands like gdscript-docs-browse-api
browses the online API reference, even if a local copy is
available."
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-docs-use-eww t
  "If set to false use the emacs configurable `browse-url'
function rather than `eww' directly. `browse-url' can be
configured to open the desktop default GUI browser, for example,
via the variable `browse-url-browser-function'"
  :type 'boolean
  :group 'gdscript)

(defcustom gdscript-docs-local-path ""
  "Optional path to a local build of the Godot documentation.
If not set to an empty string, the commands `gdscript-docs-browse-api'
and `gdscript-docs-browse-symbol-at-point' allow you to browse the local files.
Must be the root directory of the website, that is to say, a
directory path containing the file `index.html'."
  :type 'string
  :group 'gdscript)

(defcustom gdscript-docs-online-search-api-url "https://docs.godotengine.org/en/stable/search.html?q=%s&check_keywords=yes&area=default"
  "Online Godot API search url"
  :type 'string
  :group 'gdscript)

(defcustom gdscript-debug-port 6010
  "Debugger server port."
  :type 'integer
  :group 'gdscript)


(provide 'gdscript-customization)
;;; gdscript-customization.el ends here
