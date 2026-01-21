;; -*- mode: eask; lexical-binding: t -*-

(package "gdscript-mode"
         "0.1.0"
         "Major mode for Godot's GDScript language")

(website-url "https://github.com/godotengine/emacs-gdscript-mode/")
(keywords "languages")

(package-file "gdscript-mode.el")
(files "*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "26.3")

(development
 (depends-on "projectile")
 (depends-on "hydra"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             ;; TODO: Enable this once the code base is stable enough.
             ;;(setq byte-compile-error-on-warn t)
             (setq byte-compile-docstring-max-column 1000)))
