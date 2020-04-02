# Changelog

This document lists new features, improvements, changes, and bug fixes in each release of the package.

## GDScript mode 1.?.?

### Features

- Added a command to format a selected region with `gdformat`.

## GDScript mode 1.2.0

### Features

- Added a command to insert a path to a project file, either using `project-find-file` if `projectile` is available, otherwise with `find-file`.

### Bug fixes

- Fixed loading the `gdscript-godot` module at initialization.
- Fixed function calls in the mode map.

## GDScript mode 1.1.0

Emacs GDScript mode is now available on the [MELPA](https://melpa.org/) package archive!

### Features

- Added commands to run files or open the project in Godot. Type `M-x gdscript-godot` to find and try them.

### Bug fixes

- Fixed incorrect match block indentation.
- Fixed error with code folding.
- Added syntax highlighting for more built-in keywords .
- Fixed error at startup on Emacs 26.3

### Documentation

- Added instructions to install with use-package.
- Updated install instructions for MELPA.
- Added documentation on using `gdscript-godot-*` commands.

## GDScript mode 1.0.2

### Changes

- Addressed linting and checkdoc errors for a release on the [MELPA](https://melpa.org/) package archive.
- Split some more of the code to make the code easier to read and to maintain.

### Bug fixes

- Fixed `function-definition-void` errors in some cases due to a missing file import.
- Fixed package loading error in some configurations.
- Added syntax highlighting and completion for the following keywords: `in`, `and`, `or`, `not`, `true`, `false`.
- Fixed incorrect auto-indentation in `match` blocks.

## GDScript mode 1.0.1

This minor release fixes a bug with the GDScript keywords.

### Improvements

- Compile keywords for faster auto-completion and syntax highlighting.

### Bug fixes

- Fixed missing language keywords and constants lists.

## GDScript mode 1.0.0

This is the initial release of gdscript-mode, which adds support for the [Godot engine](https://godotengine.org/)'s GDScript programming language in Emacs.

Features:

- Syntax highlighting.
- Code folding.
- Imenu.
- Indentation and auto-indentation: tab-based (default) and space-based.
- GDScript code formatting using [gdformat](https://github.com/scony/godot-gdscript-toolkit/).
- Automatic pairing of parentheses, brackets, etc.
- Comment wrapping when using fill-paragraph.
- Support for scenes (.tscn), resources (.tres) and GDScript (.gd) files.
- Basic code completion.
