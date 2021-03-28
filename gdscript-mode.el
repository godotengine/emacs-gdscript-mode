;;; gdscript-mode.el --- Major mode for Godot's GDScript language -*- lexical-binding: t; -*-

;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>, Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Jan 2020
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

;; Adds support for the GDScript programming language from the Godot game
;; engine. This is a domain-specific language dedicated to game programming.

;;; Code:

(require 'gdscript-customization)
(require 'gdscript-docs)
(require 'gdscript-syntax)
(require 'gdscript-indent-and-nav)
(require 'gdscript-imenu)
(require 'gdscript-fill-paragraph)
(require 'gdscript-completion)
(require 'gdscript-format)
(require 'gdscript-rx)
(require 'gdscript-godot)
(require 'gdscript-hydra)
(require 'gdscript-debug)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tscn\\'" . conf-toml-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tres\\'" . conf-toml-mode))

(defvar gdscript-mode-map (let ((map (make-sparse-keymap)))
                            ;; Movement
                            (define-key map [remap backward-sentence] 'gdscript-nav-backward-block)
                            (define-key map [remap forward-sentence] 'gdscript-nav-forward-block)
                            (define-key map [remap backward-up-list] 'gdscript-nav-backward-up-list)
                            (define-key map [remap mark-defun] 'gdscript-mark-defun)
                            (define-key map (kbd "C-c C-j") 'imenu)
                            ;; Indent specific
                            (define-key map "\177" 'gdscript-indent-dedent-line-backspace)
                            (define-key map (kbd "<backtab>") 'gdscript-indent-dedent-line)
                            (define-key map (kbd "\t") 'company-complete)
                            ;; Insertion.
                            (define-key map (kbd "C-c i") 'gdscript-completion-insert-file-path-at-point)
                            ;; Formatting.
                            (define-key map (kbd "C-c C-f r") 'gdscript-format-region)
                            (define-key map (kbd "C-c C-f b") 'gdscript-format-buffer)
                            ;; Run in Godot.
                            (define-key map (kbd "C-c C-r p") 'gdscript-godot-open-project-in-editor)
                            (define-key map (kbd "C-c C-r r") 'gdscript-godot-run-project)
                            (define-key map (kbd "<f5>") 'gdscript-godot-run-project)
                            (define-key map (kbd "C-c C-r d") 'gdscript-godot-run-project-debug)
                            (define-key map (kbd "C-c C-r s") 'gdscript-godot-run-current-scene)
                            (define-key map (kbd "<f6>") 'gdscript-godot-run-current-scene)
                            (define-key map (kbd "C-c C-r q") 'gdscript-godot-run-current-scene-debug)
                            (define-key map (kbd "C-c C-r e") 'gdscript-godot-edit-current-scene)
                            (define-key map (kbd "C-c C-r x") 'gdscript-godot-run-current-script)
                            ;; Docs.
                            (define-key map (kbd "C-c C-b a") 'gdscript-docs-browse-api)
                            (define-key map (kbd "C-c C-b o") 'gdscript-docs-browse-symbol-at-point)
                            (define-key map (kbd "C-c C-b s") 'gdscript-docs-online-search-api)
                            ;; Hydra
                            (define-key map (kbd "C-c r") 'gdscript-hydra-show)
                            ;; Debugger
                            (define-key map (kbd "C-c C-d C-d s") 'gdscript-debug-display-stack-frame-vars-buffer)
                            (define-key map (kbd "C-c C-d C-d d") 'gdscript-debug-display-stack-dump-buffer)
                            (define-key map (kbd "C-c C-d C-d b") 'gdscript-debug-display-breakpoint-buffer)
                            (define-key map (kbd "C-c C-d C-d i") 'gdscript-debug-display-inspector-buffer)
                            (define-key map (kbd "<f9>") 'gdscript-debug-toggle-breakpoint)
                            (define-key map (kbd "C-c C-d q") 'gdscript-debug-make-server)
                            (define-key map (kbd "C-c C-d n") 'gdscript-debug-next)
                            (define-key map (kbd "C-c C-d c") 'gdscript-debug-continue)
                            (define-key map (kbd "<f7>") 'gdscript-debug-continue)
                            (define-key map (kbd "C-c C-d s") 'gdscript-debug-step)
                            ;; Debugger Hydra
                            (define-key map (kbd "C-c n") 'gdscript-debug-hydra)
                            map)
  "Keymap for `gdscript-mode'.")

(defun gdscript-hideshow-forward-sexp-function (_arg)
  "Gdscript specific `forward-sexp' function for function `hs-minor-mode'.
Argument ARG is ignored."
  (gdscript-nav-end-of-defun)
  (unless (gdscript-info-current-line-empty-p)
    (backward-char)))

(defun gdscript-electric-pair-string-delimiter ()
  "GDScript-specific string delimiter detection for 'electric-pair-mode'.
Return a doubled string character for 'electric-pair-mode', if
the last command event was a string delimiter."
  (when (and electric-pair-mode
             (memq last-command-event
                   '(?\" ?\') )
             (let ((count 0))
               (while (eq (char-before (- (point)
                                          count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion
      (insert (make-string 2 last-command-event)))))

;;;###autoload
(define-derived-mode gdscript-mode prog-mode "gdscript"
  "Major mode for editing Godot GDScript files."
  (setq-local tab-width gdscript-tab-width)
  (setq-local indent-tabs-mode gdscript-use-tab-indents)

  (set-syntax-table gdscript-mode-syntax-table)
  (modify-syntax-entry ?\# "\<" gdscript-mode-syntax-table)
  (modify-syntax-entry ?\n ">" gdscript-mode-syntax-table)

  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local forward-sexp-function
              'gdscript-nav-forward-sexp)

  (setq-local font-lock-defaults
              '(gdscript-font-lock))

  (setq-local syntax-propertize-function
              gdscript-syntax-propertize-function)

  (setq-local indent-line-function
              #'gdscript-indent-line-function)
  (setq-local indent-region-function #'gdscript-indent-region)

  ;; because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)
  (setq-local electric-indent-chars
              (cons ?: electric-indent-chars))

  ;; add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'gdscript-electric-pair-string-delimiter 'append t)

  (setq-local paragraph-start "\\s-*$")
  (setq-local fill-paragraph-function
              #'gdscript-fill-paragraph)
  (setq-local normal-auto-fill-function #'gdscript-fill-paragraph-do-auto-fill)

  (setq-local beginning-of-defun-function
              #'gdscript-nav-beginning-of-defun)
  (setq-local end-of-defun-function
              #'gdscript-nav-end-of-defun)

  (add-hook 'completion-at-point-functions
            #'gdscript-completion-at-point nil 'local)

  (add-hook 'post-self-insert-hook
            #'gdscript-indent-post-self-insert-function 'append 'local)

  (setq-local imenu-create-index-function
              #'gdscript-imenu-create-index)

  (setq-local add-log-current-defun-function
              #'gdscript-info-current-defun)

  (add-hook 'which-func-functions #'gdscript-info-current-defun nil t)

  (setq-local hs-special-modes-alist ''(gdscript-mode
                                        "\\s-*\\_<\\(?:func\\|class\\)\\_>"
                                        ;; use the empty string as end regexp so it doesn't default to
                                        ;; "\\s)".  this way parens at end of defun are properly hidden.
                                        ""
                                        "#"
                                        gdscript-hideshow-forward-sexp-function
                                        nil))

  (setq-local outline-regexp
              (gdscript-rx (* space) block-start))
  (setq-local outline-level
              #'(lambda ()
                  "`outline-level' function for gdscript mode."
                  (1+ (/ (current-indentation) gdscript-indent-offset)))))

(provide 'gdscript-mode)

;;; gdscript-mode.el ends here
