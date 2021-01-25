# Changelog

This document lists new features, improvements, changes, and bug fixes in each release of the package.

## GDScript mode 1.5.0

- Added the ability to toggle breakpoint on the current line instead of either adding or removing them.
- Added keybindings matching Godot to run the project (<kbd>f5</kbd>), current scene (<kbd>f6</kbd>), continue execution (<kbd>f7</kbd>), and toggle breakpoints <kbd>F9</kbd>.

## GDScript mode 1.4.1

Thanks to @clangdo for the contribution!

- Fixed indent functions preventing double hanging indent in parentheses or after a `\`.

## GDScript mode 1.4.0

Big thanks to @VlachJosef and @rileyrg for their work on this release.

### New features

- Debugger support with breakpoints, code stepping, stack frames, remote scene tree, and more. See the [README](https://github.com/godotengine/emacs-gdscript-mode/blob/master/README.md) for more information.

### Bug fixes

- Fix scene and script selection not aborting when pressing <kbd>C-g</kbd>.

## GDScript mode 1.3.0

This release brings many quality-of-life improvements to work more productively with Godot and Emacs.

Big thanks to @VlachJosef and @rileyrg for contributing to this release.

### Features

- Support for running the project and scenes with [hydra](https://github.com/abo-abo/hydra) with `gdscript-hydra-show`.
- Command interpreter (`comint` support) for Godot processes: get and navigate errors within Emacs, and jump to files and GDScript code causing errors.
  - The command buffer pops up automatically when you run a scene or the project in the editor.
- Added command `gdscript-format-all` to format all gdscript buffers.
- Added commands `gdscript-format-all` and `gdscript-format-buffer` to gdscript-mode's hydra window (`gdscript-hydra-show`).
- Added command interpreter support for `gdscript-format-*` commands
- Add the ability to open a local copy of the Godot docs with `gdscript-docs-*` commands.
- Multiple projects support. Every project's `godot` process runs in its own buffer.
- Godot's standard output and standard error are fed to a `comint` buffer. This allows you to navigate errors and jump to the corresponding source files, using `compilation-*` commands.
- Hydra provides a history of commands for quick re-execution of godot commands. It also provides a quick way to rerun the last command.
- The `gdscript-godot-run-current-scene` command now offers to run any scene file if the current buffer is not a scene file.
- The `gdscript-godot-run-current-script` command now offers to run any script file if the current buffer is not a script file.
- Syntax highlighting for the `$` operator.

### Improvements

- Added unit tests (see `gdscript-tests.el`).
- Added check for missing `godot` executable and a corresponding error message.
- You can force selecting a scene when calling `gdscript-godot-run-current-scene` by using the universal argument (<kbd>C-u</kbd>).
- `gdscript-godot-run-current-scene` will use `projectile` by default if available, otherwise `ivy` or `ido`.
- You can now customize the URL for the Godot API reference: `gdscript-docs-online-search-api-url`.
  - Also, use the universal argument (<kbd>C-u</kbd>) before calling `gdscript-docs-browse-api` to force it to use the online docs, even if a local build of the docs is available.

### Changes

- Removed guessing indentation size, which could guess indent sizes wrong.
- Removed the customizable variables `gdscript-indent-guess-indent-offset` and `gdscript-indent-guess-indent-offset-verbose`.

### Bug fixes

- Fixed auto-indentation not working with match blocks.
- Fixed auto-indentation of new blocks sometimes over-indenting.
- Fixed `eww-after-render-hook` always calling gdscript docs formatter, even outside of `gdscript-mode` buffers.
- Removed call to nonexisting function `f-executable-p`.

## GDScript mode 1.2.0

### Features

- Added commands to open the API reference in `eww`.
- Added debug options when running `gdscript-godot-run-project-debug`.
- Added a command to insert a path to a project file, either using `project-find-file` if `projectile` is available, otherwise with `find-file`.
- Added a command to format a selected region with `gdformat`.
- Added syntax highlighting for function calls.
- Added missing built in functions.
- Added missing `puppet` and `remotesync` keywords.

### Changes

- Changed keyboard shortcuts:
  - <kbd>C-c i</kbd> `gdscript-completion-insert-file-path-at-point`
  - <kbd>C-c C-f r</kbd> `gdscript-format-region`
  - <kbd>C-c C-f b</kbd> `gdscript-format-buffer`
  - <kbd>C-c C-r p</kbd> `gdscript-godot-open-project-in-editor`
  - <kbd>C-c C-r r</kbd> `gdscript-godot-run-project`
  - <kbd>C-c C-r d</kbd> `gdscript-godot-run-project-debug`
  - <kbd>C-c C-r s</kbd> `gdscript-godot-run-current-scene`
  - <kbd>C-c C-r q</kbd> `gdscript-godot-run-current-scene-debug`
  - <kbd>C-c C-r e</kbd> `gdscript-godot-edit-current-scene`
  - <kbd>C-c C-r x</kbd> `gdscript-godot-run-current-script`
  - <kbd>C-c C-b a</kbd> `gdscript-docs-browse-api`
  - <kbd>C-c C-b o</kbd> `gdscript-docs-browse-symbol-at-point`

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
