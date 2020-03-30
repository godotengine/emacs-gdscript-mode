# GDScript mode for Emacs

![banner showing the "GDScript mode" title with GDScript code in the
background](assets/banner.svg)

This package adds support for the GDScript programming language from the Godot
game engine in Emacs. It gives syntax highlighting and indentations.
[Contributors](#contributing) are welcome!

## Features

This mode already features all the essentials:

- Syntax highlighting.
- Code folding.
- [Imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html).
- Support for scenes (`.tscn`) and script (`.gd`) files.
- Comment wrapping when using `fill-paragraph`.
- Indentation and auto-indentation: tab-based (default) and space-based.
- Automatic pairing of parentheses, brackets, etc.
- Code formatting using
  [gdformat](https://github.com/scony/godot-gdscript-toolkit/).
- Auto-completion for all the keywords in the `gdscript-keywords.el` file.
- Run or open the project and files with Godot.

## Contributing

Contributors are welcome! Check the [issues tab](issues) for tasks to work on and open a PR anytime.

If you find a bug, or would like to suggest an improvement, [open a new
issue](issues/new).

For code style, we follow the [Emacs lisp style
guide](https://github.com/bbatsov/emacs-lisp-style-guide) by Bozhidar Batsov,
and the [tips and
conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)
from the Emacs manual.

You should also check for errors and linter warnings in your code. You can do so in Emacs with flymake or flycheck but we recommend running the tool `makem.sh` provided with the repository:

```sh
./makem.sh lint-compile
```

This program will tell you if there is any problem with your code. If there's no output, everything is fine. You can run all tests like so, but note it might give you spelling errors that aren't relevant in this project:

```sh
./makem.sh all
```

## How to install

The package is available in the [MELPA](https://melpa.org/#/) package archive. Once you [set up MELPA](https://melpa.org/#/getting-started) you can install the package from Emacs:

```lisp
M-x package-install gdscript-mode
```

Then, in your init.el file, you can require the package:

```lisp
(require 'gdscript-mode)
```

### Installing in Spacemacs

1. Clone the repository to the `private/local` subdirectory of your `.emacs.d`
   directory, where you installed spacemacs.
2. Add the package to the `dotspacemacs-additional-packages` and mark it as
   local. That's Spacemacs' feature to make it easy to load locally installed
   packages.

```lisp
dotspacemacs-additional-packages '((gdscript-mode :location local))
```

3. In your `dotspacemacs/user-config` function, require the package.

```lisp
(defun dotspacemacs/user-config ()
  (require 'gdscript-mode))
```

### Installing in Doom Emacs

Add the following package definition to your `.doom.d/packages.el` file:

```lisp
(package! gdscript-mode
          :recipe (:host github
                   :repo "GDQuest/emacs-gdscript-mode"))
```

Require the package in your `.doom.d/config.el` file:

```lisp
(require 'gdscript-mode)
```

### Installing with `use-package` + `straight.el`

Add the call to use-package to your Emacs configuration:

```lisp
(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "GDQuest/emacs-gdscript-mode"))
```

### Installing manually

1. Clone the repository or download a [stable release](https://github.com/GDQuest/emacs-gdscript-mode/releases) to your computer.
1. In your init.el file, add a call to load and require the package.

```lisp
(add-to-list 'load-path "/path/to/gdscript-mode")
(require 'gdscript-mode)
```

## How to use

### Opening the project in the editor

You can open the project in the Godot editor with `M-x gdscript-godot-open-project-in-editor`, or open files and more in Godot with the `M-x gdscript-godot-*` commands.

By default, these commands try to use an executable named `godot` on the system [PATH environment variable](<https://en.wikipedia.org/wiki/PATH_(variable)>).

If you don't have `godot` available there, you can set a custom executable name or path to use instead:

```lisp
(setq gdscript-godot-executable "/path/to/godot")
```

You can also use `customize` to change this path: `M-x customize` and search for "godot".

### Formatting code with gdformat

You can call the `gdscript-format` function to format the current buffer with
`gdformat`. This feature requires the python package `gdtoolkit` to be installed
and available on the system's PATH variable.

You can install gdtoolkit using the pip package manager from Python 3. Run this
command in your shell to install it:

```
pip3 install gdtoolkit
```

## Customization

To find all GDScript-mode settings, press `M-x customize` and search for "gdscript".

Code example:

```lisp
(setq gdscript-use-tab-indents t) ;; If true, use tabs for indents. Default: t
(setq gdscript-indent-offset 4) ;; Controls the width of tab-based indents
(setq gdscript-godot-executable "/path/to/godot") ;; Use this executable instead of 'godot' to open the Godot editor.
```
