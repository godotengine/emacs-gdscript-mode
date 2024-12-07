;;; gdscript-eglot.el --- Integration with eglot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 GDQuest and contributors
;;
;; Author: Ruijie Yu <ruijie@netyu.xyz>
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
;; Handles the configuraiton of eglot.
;; This supports `gdscript-mode' using `eglot'.
;;
;;; Code:

;;;###autoload
(defgroup gdscript-eglot nil
  "Configurations in gdscript related to `eglot'."
  :group 'gdscript)

;;;###autoload
(defcustom gdscript-eglot-version "4.3"
  "The version of godot in use."
  :type 'string)

(defun gdscript-eglot--get-config-dir ()
  "Get system-specific directory with Godot configuration files."
  (pcase system-type
    ('darwin "~/Library/Application Support/Godot/")
    ('windows-nt (substitute-in-file-name "$APPDATA/Godot/"))
    ('gnu/linux (file-name-concat
                 (or (getenv "XDG_CONFIG_HOME") "~/.config/")
                 "godot"))))

(defun gdscript-eglot--extract-port (editor-settings-file)
  "Extract LSP port from Godot editor settings file."
  (when (file-exists-p editor-settings-file)
    (with-temp-buffer
      (insert-file-contents editor-settings-file)
      (when (re-search-forward
             (rx "network/language_server/remote_port"
                 (* space) ?= (* space)
                 (group (+ digit)))
             nil t)
        (string-to-number (match-string 1))))))

;;;###autoload
(defun gdscript-eglot-contact (_interactive)
  "Attempt to help `eglot' contact the running gdscript LSP.
Returns a list (HOST PORT) if successful, nil otherwise.  See the
last definition of CONTACT in `eglot-server-programs' for
definitions of HOST, PORT, and INTERACTIVE.

For more context, see
https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-04/msg01070.html."
  (save-excursion
    (let* ((config-dir (gdscript-eglot--get-config-dir))
           (settings-file (file-name-concat
                           config-dir
                           (format "editor_settings-%s.tres" gdscript-eglot-version))))
      (when-let ((port (gdscript-eglot--extract-port settings-file)))
        (list "localhost" port)))))

(provide 'gdscript-eglot)
;;; gdscript-eglot.el ends here.
