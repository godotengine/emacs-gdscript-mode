;;; gdscript-format.el --- GDScript formatting with gdformat -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest, Pawel Lampe

;; Author: Pawel Lampe <pawel.lampe@gmail.com>, Nathan Lovato <nathan@gdquest.com>
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

;; Format a buffer containing GDScript code with the gdformat GDScript formatter.
;; This code is derived from gdtoolkit, see https://github.com/Scony/godot-gdscript-toolkit

;;; Code:

(defun gdscript-format--run-gdformat (buffer-input buffer-output buffer-error)
  "Call gdformat process.
Argument BUFFER-INPUT reference to the input buffer to format.
Argument BUFFER-OUTPUT the buffer to write the output of the gdformat call.
Argument BUFFER-ERROR the buffer to write errors to."
  (with-current-buffer buffer-input
    (let ((process (make-process :name "gdformat"
                                 :command (list "gdformat" "-"):buffer
                                 buffer-output
                                 :stderr buffer-error
                                 :noquery t
                                 :sentinel (lambda (_process _event)))))
      (set-process-query-on-exit-flag (get-buffer-process buffer-error)
                                      nil)
      (set-process-sentinel (get-buffer-process buffer-error)
                            (lambda (_process _event)))
      (save-restriction (widen)
                        (process-send-region process
                                             (point-min)
                                             (point-max)))
      (process-send-eof process)
      (accept-process-output process nil nil t)
      (while (process-live-p process)
        (accept-process-output process nil nil t))
      (process-exit-status process))))

(defun gdscript-format-buffer ()
  "Formats current buffer using 'gdformat'."
  (interactive)
  (let* ((buffer-start (current-buffer))
         (point-start (point))
         (window-pos-start (window-start))
         (buffer-temp (get-buffer-create "*gdformat*"))
         (buffer-error (get-buffer-create "*gdformat-error*")))
    (dolist (buf (list buffer-temp buffer-error))
      (with-current-buffer buf
        (erase-buffer)))
    (condition-case err
        (if (/= 0 (gdscript-format--run-gdformat buffer-start buffer-temp
                                         buffer-error))
            (error "GDSCript formatter: failed, see buffer %s for details"
                   (buffer-name buffer-error))
          (if (/= 0 (compare-buffer-substrings buffer-temp nil
                                               nil buffer-start nil nil))
              (progn
                (with-current-buffer buffer-temp
                  (copy-to-buffer buffer-start
                                  (point-min)
                                  (point-max)))
                (goto-char point-start)
                (set-window-start (selected-window)
                                  window-pos-start)
                (message "gdscript formatter: success"))
            (message "gdscript formatter: nothing to do"))
          (mapc 'kill-buffer
                (list buffer-temp buffer-error)))
      (error (message "%s"
                      (error-message-string err))
             (pop-to-buffer buffer-error)))))

(provide 'gdscript-format)

;;; gdscript-format.el ends here
