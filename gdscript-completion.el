;;; gdscript-completion.el --- Autocompletion for GDScript  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 GDQuest and contributors

;; Author: Nathan Lovato <nathan@gdquest.com>
;;         Fabián E. Gallina <fgallina@gnu.org>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>

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
;;
;; Provides keywords for 'completion-at-point'.
;;

;;; Code:

(require 'gdscript-syntax)
(require 'gdscript-utils)
(require 'projectile nil t)

(declare-function projectile-project-files nil "ext:projectile.el")
(declare-function projectile-completing-read nil "ext:projectile.el")
(declare-function projectile-project-root nil "ext:projectile.el")
(declare-function projectile-ensure-project nil "ext:projectile.el")
(declare-function projectile-maybe-invalidate-cache nil "ext:projectile.el")

(declare-function project-current nil "project")
(declare-function project-root nil "project")
(declare-function project-files nil "project")

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

(defun gdscript-completion-insert-file-path-at-point (&optional arg)
  "Insert a file path at point using Godot's relative path (\"res:\").

If Projectile is available, list only the files in the current
project.  If project.el detects a project, use that instead.
Otherwise, fallback to the built-in function `read-file-name'.

If using Projectile, with a prefix ARG invalidates the cache
first."
  (interactive "P")
  (let* ((has-projectile (featurep 'projectile))
         (has-project-el (and (featurep 'project) (project-current)))
         (project-root
          (cond (has-projectile
                 (projectile-maybe-invalidate-cache arg)
                 (projectile-ensure-project (projectile-project-root)))
                (has-project-el
                 (project-root (project-current)))
                (t
                 (gdscript-util--find-project-configuration-file))))
         (file
          (when project-root
            (cond (has-projectile
                   (projectile-completing-read
                    "Find file: "
                    (projectile-project-files project-root)))
                  (has-project-el
                   (completing-read
                    "Find file: "
                    (mapcar (lambda (f)
                              (file-relative-name f project-root))
                            (project-files (project-current)))))
                  (t
                   (read-file-name "Find file: " project-root)))))
         (resource-path
          (when file
            (cond ((or has-projectile has-project-el)
                   file)
                  (t
                   (concat (gdscript-util--get-godot-project-file-path-relative file)
                           "." (file-name-extension file)))))))
    (when resource-path
      (insert (concat "\"res://" resource-path "\"")))))

(provide 'gdscript-completion)
;;; gdscript-completion.el ends here
