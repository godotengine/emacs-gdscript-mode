;;; gdscript--xml-docs.el --- Open local xml documentation in Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Chang Xiaoduan <drcxd@sina.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: drcxd@sina.com
;; Created: April 2023
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
;;  Parse the xml doc files come along with the engine source code and
;;  display the content in an org-mode buffer.
;;
;;; Code:

(require 'dom)
(require 'gdscript-customization)

(defun gdscript-xml-docs--open (file)
  "Open a godot xml doc file and print its content in a org-mode
buffer."
  (interactive "fXml doc file: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((dom-node (libxml-parse-xml-region)))
      (with-output-to-temp-buffer "*gd-xml-doc*"
        (with-current-buffer "*gd-xml-doc*"
          (gdscript-xml-docs-print-node dom-node 1)
          (org-mode)
          (read-only-mode t))))))

;;;###autoload
(defun gdscript-xml-docs-open ()
  "Open a godot xml doc file and print its content in a org-mode
buffer. Browsing a default directory."
  (interactive)
  (let ((default-directory gdscript-xml-docs-local-path))
    (call-interactively 'gdscript-xml-docs--open)))

(defun gdscript-xml-docs-print-node (node depth)
  "Iterate through the doc tree and print each node accordingly."
  (let ((tag (if (stringp node) 'plain-string (dom-tag node))))
    (cond ((eq tag 'class) (gdscript-xml-docs-print-class-node node depth))
          ((eq tag 'brief_description) (gdscript-xml-docs-print-brief-desc-node node depth))
          ((eq tag 'description) (gdscript-xml-docs-print-desc-node node depth))
          ((eq tag 'plain-string) (gdscript-xml-docs-print-plain-string-node node depth))
          ((eq tag 'tutorials) (gdscript-xml-docs-print-tutorials-node node depth))
          ((eq tag 'members) (gdscript-xml-docs-print-members-node node depth))
          ((eq tag 'member) (gdscript-xml-docs-print-member-node node depth))
          ((eq tag 'signals) (gdscript-xml-docs-print-signals-node node depth))
          ((eq tag 'signal) (gdscript-xml-docs-print-signal-node node depth))
          ((eq tag 'link) (gdscript-xml-docs-print-link-node node depth))
          ((eq tag 'constructors) (gdscript-xml-docs-print-constructors-node node depth))
          ((eq tag 'constructor) (gdscript-xml-docs-print-constructor-node node depth))
          ((eq tag 'methods) (gdscript-xml-docs-print-methods-node node depth))
          ((eq tag 'method) (gdscript-xml-docs-print-method-node node depth))
          ((eq tag 'operators) (gdscript-xml-docs-print-operators-node node depth))
          ((eq tag 'operator) (gdscript-xml-docs-print-operator-node node depth))
          ((eq tag 'constants) (gdscript-xml-docs-print-constants-node node depth))
          (t (princ (format "Unhandled node tag: %s\n" tag))))
    (if (not (or (eq tag 'plain-string)
                 (eq tag 'link)
                 (eq tag 'constructor)
                 (eq tag 'method)
                 (eq tag 'operator)
                 (eq tag 'constants)))
        (dolist (child (dom-children node))
          (gdscript-xml-docs-print-node child (+ 1 depth))))))

;;; printers

;; TODO: handle "inherits"
(defun gdscript-xml-docs-print-title-node (node depth)
  (princ (format "\n%s %s\n" (make-string depth ?*) (dom-tag node))))

(defun gdscript-xml-docs-print-class-node (node depth)
  (princ (format "%s %s\n" (make-string depth ?*) (dom-attr node 'name))))

(defun gdscript-xml-docs-print-brief-desc-node (node depth))

(defun gdscript-xml-docs-print-desc-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

(defun gdscript-xml-docs-print-plain-string-node (node depth)
  (gdscript-xml-docs-print-parsed-node (gdscript-xml-docs-parse-string node nil)))

(defun gdscript-xml-docs-print-tutorials-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

(defun gdscript-xml-docs-print-members-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

(defun gdscript-xml-docs-print-constructors-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

;; TODO: print setters and getters
(defun gdscript-xml-docs-print-member-node (node depth)
  (princ (format "\n+ =%s %s = %s=\n"
                 (dom-attr node 'type)
                 (dom-attr node 'name)
                 (dom-attr node 'default))))

(defun gdscript-xml-docs-print-signals-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

(defun gdscript-xml-docs-print-signal-node (node depth)
  (princ (format "\n+ =%s()=\n" (dom-attr node 'name))))

(defun gdscript-xml-docs-print-link-node (node depth)
  (princ (format "[[%s][%s]]\n"
                 (dom-texts node)
                 (dom-attr node 'title))))

(defun gdscript-xml-docs-print-constructor-node (node depth)
  (gdscript-xml-docs-print-function-node node depth))

(defun gdscript-xml-docs-print-function-node (node depth)
  (princ (format "\n+ =%s %s(%s)=\n"
                 (dom-attr (elt (dom-by-tag node 'return) 0) 'type)
                 (dom-attr node 'name)
                 (let* ((params (dom-by-tag node 'param))
                        (length (length params))
                        (str ""))
                   (if (> length 0)
                       (progn
                         (dotimes (i (- length 1))
                           (setq str (concat str (format "%s : %s, "
                                                         (dom-attr (elt params i) 'name)
                                                         (dom-attr (elt params i) 'type)))))
                         (setq str (concat str (format "%s : %s"
                                                       (dom-attr (elt params (- length 1)) 'name)
                                                       (dom-attr (elt params (- length 1)) 'type))))))
                   str)))
  (gdscript-xml-docs-print-parsed-node (gdscript-xml-docs-parse-string (dom-texts node) nil)))

(defun gdscript-xml-docs-print-methods-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

(defun gdscript-xml-docs-print-method-node (node depth)
  (gdscript-xml-docs-print-function-node node depth))

(defun gdscript-xml-docs-print-operators-node (node depth)
  (gdscript-xml-docs-print-title-node node depth))

(defun gdscript-xml-docs-print-operator-node (node depth)
  (gdscript-xml-docs-print-function-node node depth))

(defun gdscript-xml-docs-print-constants-node (node depth)
  (gdscript-xml-docs-print-title-node node depth)
  (let ((enum ""))
    (dolist (child (dom-children node))
      (if (not (string= enum (dom-attr child 'enum)))
          (princ (format "+ =enum %s=\n" (dom-attr child 'enum))))
      (setq enum (dom-attr child 'enum))
      (princ (format "  + =%s = %s=\n"
                     (dom-attr child 'name)
                     (dom-attr child 'value)))
      (princ (format "%s\n" (dom-texts child))))))

(defun gdscript-xml-docs-parse-string (str root)
  "Iterate through a doc string. Parse and construct a tree
structure for future use."
  (let ((len (length str))
        (beg 0)
        (end 0))
    (if (not root)
        (setq root (dom-node 'root)))
    (while (< end len)
      (let ((c (aref str end)))
        (cond ((char-equal c ?\[)
               (let ((plain-text (substring str beg end))
                     (new-node (dom-node 'text)))
                 (if (not (string= plain-text ""))
                     (progn (dom-append-child new-node plain-text)
                            (dom-append-child root new-node))))
               (setq beg end
                     end (1+ end))
               (while (not (char-equal ?\] (aref str end)))
                 (setq end (1+ end)))
               (let ((type (substring str (1+ beg) end))
                     (url ""))
                 (when (string-match "^url=" type)
                   (setq url (substring type 4)
                         type "url"))
                 (if (or (string= type "codeblocks")
                         (string= type "code")
                         (string= type "b")
                         (string= type "url")
                         (string= type "gdscript")
                         (string= type "csharp")
                         (string= type "codeblock"))
                     (progn (setq beg (1+ end)
                                  end beg)
                            (let* ((pattern (format "\\[/%s\\]" type))
                                   (close-index (string-match pattern str beg)))
                              (let ((new-node (dom-node type)))
                                (if (string= type "codeblocks")
                                    (gdscript-xml-docs-parse-string (substring str beg close-index) new-node)
                                  (dom-append-child new-node (substring str beg close-index)))
                                (if (string= type "url")
                                    (dom-set-attribute new-node 'url url))
                                (dom-append-child root new-node))
                              (setq beg (+ close-index (- (length pattern) 2)) ;; minus 2 because we are quoting the two \
                                    end beg)))
                   (let ((new-node (dom-node 'block)))
                     (dom-append-child new-node type)
                     (dom-append-child root new-node)
                     (setq beg (1+ end)
                           end beg)))))
              (t (setq end (1+ end))))))
    (let ((new-node (dom-node 'text))
          (remain (substring str beg)))
      (dom-append-child new-node remain)
      (dom-append-child root new-node))
    root))

(defun gdscript-xml-docs-print-parsed-node (root)
  "Print the tree generated by
`gdscript-xml-docs-parse-string'. Using org-mode marking syntax."
  (let ((tag (dom-tag root))
        (text (dom-text root)))
    (cond
     ((or (eq 'root tag) (string= "codeblocks" tag))
      (dolist (child (dom-children root))
        (gdscript-xml-docs-print-parsed-node child)))
     ((string= "b" tag) (princ (format "​*%s*​" text))) ;; wrapped with zero-width space
     ((string= "code" tag) (princ (format "​=%s=​" text))) ;; wrapped with zero-width space
     ((string= "url" tag) (princ (format "[[%s][%s]]" (dom-attr root 'url) text)))
     ((string= "gdscript" tag) (princ (format "\n#+begin_src gdscript\n%s\n#+end_src\n" text)))
     ((string= "csharp" tag) (princ (format "\n#+begin_src csharp\n%s\n#+end_src\n" text)))
     ((string= "block" tag) (princ (format "[%s]" text)))
     ((string= "text" tag) (dolist (line (string-lines text nil t))
                             (princ (format "%s" (string-trim-left line "[\t]+")))))
     ((string= "codeblock" tag) (princ (format "\n#+begin_src gdscript\n%s\n#+end_src\n" text))))))

(provide 'gdscript-xml-docs)
;;; gdscript-xml-docs.el ends here
