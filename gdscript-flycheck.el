;;; gdscript-flycheck.el --- GDScript formatting with gdformat -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Pawel Lampe

;; Author: Oliver Frank <oliverfrank321@gmail.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: oliverfrank321@gmail.com
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

;; Contains linting support provided by the GDScript toolkit as well.
;; This code is derived from gdtoolkit, see https://github.com/Scony/godot-gdscript-toolkit

;;; Code:

;; Must require for flycheck-define-checker to function properly
(require 'flycheck)

(flycheck-define-checker gdscript-lint
  "A GDScript linter using gdlint.
See URL https://github.com/Scony/godot-gdscript-toolkit"
    :command ("gdlint" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": Error: " (message) line-end))
    :modes gdscript-mode)
(add-to-list 'flycheck-checkers 'gdscript-lint)

(setq lsp-flycheck-live-reporting nil)
(add-hook 'lsp-ui-mode-hook
          (lambda () (flycheck-add-next-checker 'lsp-ui '(warning . gdscript-lint))))

(provide 'gdscript-flycheck)
;;; gdscript-flycheck.el ends here
