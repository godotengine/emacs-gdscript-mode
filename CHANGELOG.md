# Changelog #

This document lists new features, improvements, changes, and bug fixes in each release of the package.

## GDScript mode 1.0.1 ##

This minor release fixes a bug with the GDScript keywords.

### Improvements ###

- Compile keywords for faster auto-completion and syntax highlighting.

### Bug fixes ###

- Fixed missing language keywords and constants lists.

## GDScript mode 1.0.0 ##

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
