# GDScript mode for Emacs #

This package adds support for the GDScript programming language from the Godot game engine in Emacs. It gives syntax highlighting and indentations.

This is currently a work in progress, and my first time creating an Emacs package. Feedback, tips, and contributions are more than welcome!


## How to install ##

As it is in development, the package is not available yet on Emacs package managers.

To install it:

1. Clone the repository to your computer.
1. In your init.el file, add a call to load and require the package.

```lisp
(add-to-list 'load-path "/path/to/gdscript-mode.el")
(require 'gdscript-mode)
```

### Installing in Spacemacs ###

1. Clone the repository to the `private/local` subdirectory of your `.emacs.d` directory, where you installed spacemacs.
2. Add the package to the `dotspacemacs-additional-packages` and mark is as local. That's Spacemacs' feature to make it easy to load locally installed packages. 

```lisp
dotspacemacs-additional-packages '((gdscript-mode :location local))
```

3. In your user-config function, require the package.

```lisp
(defun dotspacemacs/user-config ()
  (require 'gdscript-mode))
```

### Installing in Doom Emacs ###

1. Add the following line to your .doom.d/packages.el file

```lisp
(package! gdscript-mode :recipe (:host github :repo "GDQuest/emacs-gdscript-mode"))
```

## Available features ##

- Syntax highlighting for all built-in functions and classes in Godot 3.2
- Indentation
- Imenu to jump to functions quickly

## Planned features ##

1. Fixing the support for indentations.
1. Functions to open the project in Godot.

For auto-competition, we should rely on the GDScript language server coming in Godot 3.2. Another developer is working on it.

## Customization ##

Set the following variables to customize gdscript-mode:

```lisp
(setq gdscript-tabs-mode t) ;; If true, use tabs for indents. Default: t
(setq gdscript-tab-width 4) ;; Controls the width of tab-based indents
```


## Credits ##

This is based on:

1. The [gdscript-mode](https://github.com/akoaysigod/gdscript-mode) package that is not in active development anymore.
1. Python support from Emacs 27. GDScript took inspiration from Python so a lot of the Python package works well for it.
