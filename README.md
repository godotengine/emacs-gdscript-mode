# GDScript mode for Emacs

![banner showing the "GDScript mode" title with GDScript code in the
background](assets/banner.png)

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
- Browsing the API reference in Emacs.

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

1. Add the package to the `dotspacemacs-additional-packages`. You can find it under the dotspacemacs/layers function:

```lisp
(defun dotspacemacs/layers ()
  "Configuration Layers declaration..."
  (setq-default
   ;; ...
   dotspacemacs-additional-packages '(gdscript-mode)
   ;; ...
   ))
```

2. In your `dotspacemacs/user-config` function, require the package.

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

### Running Godot with visual debug options

When running `gdscript-godot-run-project-debug`, you can use the universal argument <kbd>C-u</kbd> to invoke a mini-buffer with extra options to pass to godot.

Here are the available options:

1. `<no options>` _(default)_
2. `--debug-collisions`
3. `--debug-navigation`
4. `--debug-collisions --debug-navigation`

The last selected option is saved for the next time you call `gdscript-godot-run-project-debug`. To remove debug options, you need to call the command with the universal argument again.

### Using Hydra

Running `gdscript-hydra-show` (<kbd>C-c r</kbd>) opens a [hydra](https://github.com/abo-abo/hydra) popup with options to open the editor or run the project, a scene, or a script, including with visual debug options.

```
d ( ) Debug   p run project  t run script  h run from history   a format all    q quit
e ( ) Editor  s run scene    r run last    g switch to *godot*  b format buffer

c [ ] Visible collisions shapes
n [ ] Visible navigation
```

### Formatting code with gdformat

You can call the `gdscript-format` function to format the current buffer with
`gdformat`. Alternatively `gdscript-format-all` will reformat all gdscripts in
the project. This feature requires the python package `gdtoolkit` to be installed
and available on the system's PATH variable.

You can install gdtoolkit using the pip package manager from Python 3. Run this
command in your shell to install it:

```
pip3 install gdtoolkit
```

### Browsing the Godot API with eww

With the point on a built-in class you can press `C-c C-b o` to open the code reference for that class in the text browser [eww](https://www.gnu.org/software/emacs/manual/html_node/emacs/EWW.html).

To open the main API reference page and browse it, press `C-c C-b a`.

#### Using a local copy of the Godot docs

You can browse the API reference offline with `eww`. To do so:

1. Get a build of the official documentation. You can build it from the [godot docs repository](https://github.com/godotengine/godot-docs/) or [download a build](https://hugo.pro/projects/godot-builds/) from Hugo Lourcio's website.
2. Set `gdscript-docs-local-path` to the docs' directory, that contains the docs' `index.html` file.

For example:

```lisp
(setq gdscript-docs-local-path "/home/gdquest/Documents/docs/godot")
```

## Keyboard shortcuts

The following shortcuts are available by default:

- Inserting:
  - <kbd>C-c i</kbd> `gdscript-completion-insert-file-path-at-point`
- Formatting:
  - <kbd>C-c C-f r</kbd> `gdscript-format-region`
  - <kbd>C-c C-f b</kbd> `gdscript-format-buffer`
- Running the project and scenes in Godot:
  - <kbd>C-c C-r p</kbd> `gdscript-godot-open-project-in-editor`
  - <kbd>C-c C-r r</kbd> `gdscript-godot-run-project`
  - <kbd>C-c C-r d</kbd> `gdscript-godot-run-project-debug`
  - <kbd>C-c C-r s</kbd> `gdscript-godot-run-current-scene`
  - <kbd>C-c C-r q</kbd> `gdscript-godot-run-current-scene-debug`
  - <kbd>C-c C-r e</kbd> `gdscript-godot-edit-current-scene`
  - <kbd>C-c C-r x</kbd> `gdscript-godot-run-current-script`
- Browsing the code reference:
  - <kbd>C-c C-b a</kbd> `gdscript-docs-browse-api`
  - <kbd>C-c C-b o</kbd> `gdscript-docs-browse-symbol-at-point`
- Open hydra:
  - <kbd>C-c r</kbd> `gdscript-hydra-show` (require hydra package to be installed)
  - <kbd>C-c n</kbd> `gdscript-debug-hydra` (require hydra package to be installed)

## Customization

To find all GDScript-mode settings, press `M-x customize` and search for "gdscript".

Code example:

```lisp
(setq gdscript-use-tab-indents t) ;; If true, use tabs for indents. Default: t
(setq gdscript-indent-offset 4) ;; Controls the width of tab-based indents
(setq gdscript-godot-executable "/path/to/godot") ;; Use this executable instead of 'godot' to open the Godot editor.
(setq gdscript-gdformat-save-and-format t) ;; Save all buffers and format them with gdformat anytime Godot executable is run.
```

## Debugger

Use <kbd>C-c C-d b</kbd> to add breakpoint to your GDScript file. Red dot will appear in the left fringe to indicate position where breakpoint is placed.
Use <kbd>C-c C-d r</kbd> to remove breakpoint.

Once first breakpoint is added to the project, buffer named `* Breakpoints *` is created. This buffer displays all existing breakpoints in a project. Pressing <kbd>D</kbd> on breakpoint line deletes the breakpoint. Pressing <kbd>RET</kbd> on breakpoint line shows GDScript file with that breakpoint in other buffer.

When any breakpoint exists, running project will automatically start debugger server (if one isn't already running) and connect to it.
Debugger server runs on `localhost` with port specified by `gdscript-debug-port` customizable variable (`9010` by default).

Once breakpoint is hit in your code, Emacs will show two special buffers with information related to a position of the breakpoint:

- `* Stack frame vars *` - Display locals/members/globals variables for current stack point. It show variable name, its type and its value.
- `* Inspector *` - Display detailed information about selected `ObjectId`. By default it show `self` reference.

Any `ObjectId` in those two buffers can be inspected by pressing <kbd>RET</kbd> when point is on corresponding line.

### Oneline/multiline display

Variable values of types `Dictionary`, `PoolRealArray`, `PoolStringArray`, `PoolVector2Array`, `PoolVector3Array` and `PoolColorArray` could be toggled from one line display to multiline display by pressing `TAB` on corresponding line.

### Fetching `ObjectId` details

Pressing <kbd>d</kbd> in `* Stack frame vars *` or `* Inspector *` buffers (or on Debug Hydra) will fetch on the background data for all `ObjectId`s present in those two buffers and once all `ObjectId`s data are fetched, these two buffer are redisplayed. This redisplay will contain two additional informations about given `ObjectId`:
 - real type (for example `KinematicBody2D` instead of `ObjectId`)
 - node path

Also fetched data are cached and not fetched again when said `ObjectId` is inspected later on (until new breakpoint is hit).

On breakpoint hit Debug Hydra will be displayed below `* Stack frame vars *` and `* Inspector *` buffers.

### Debug Hydra

```
n next  c continue  m step  b breakpoints  s stack  v vars  i inspector  t scene-tree  d details  q quit
```

 - <kbd>n</kbd> - Jump to next line and stops there
 - <kbd>c</kbd> - Continue program execution until another breakpoint is hit
 - <kbd>m</kbd> - Step into
 - <kbd>s</kbd> - Show `* Stack dump *` buffer
 - <kbd>v</kbd> - Show `* Stack frame vars *` buffer
 - <kbd>i</kbd> - Show `* Inspector *` buffer
 - <kbd>t</kbd> - Show `* Scene tree *` buffer
 - <kbd>d</kbd> - Fetch details for all `ObjectId`s present in `* Stack frame vars *` and `* Inspector *` buffers and redisplay them.
 - <kbd>q</kbd> - Close Debug Hydra

### `* Stack frame vars *` buffer

Main source of information about running program. Contains information about locals/members/globals variables.

- Press <kbd>TAB</kbd> to toggling oneline/multiline display for selected types
- Press <kbd>RET</kbd> on `ObjectId` line to display its details
- Press <kbd>l</kbd> to display `* Stack dump *` buffer
- Press <kbd>d</kbd> to display additional details for `ObjectId` variables
- Press <kbd>p</kbd> to go to previous line
- Press <kbd>n</kbd> to go to next line
- Press <kbd>q</kbd> to close the buffer

### `* Inspector *` buffer

Contains information about inspected object. By default `self` variable from `* Stack frame vars *` is displayed. Inspected object is kept to be focused until other object is inspected or until inspected object cease to exists, in which case current `self` is displayed instead.

- Press <kbd>TAB</kbd> to toggling oneline/multiline display for selected types
- Press <kbd>RET</kbd> on `ObjectId` line to display its details
- Press <kbd>RET</kbd> on `Node/path` line (second line from the top) to show given `ObjectId` in `* Scene Tree *` buffer
- Press <kbd>l</kbd> deep in nested `ObjectId` to navigate one level up in the structure (ie. back). Pressing `l` while on top level object displays `* Stack frame vars *` buffer
- Press <kbd>d</kbd> to display additional details for `ObjectId` variables
- Press <kbd>p</kbd> to go to previous line
- Press <kbd>n</kbd> to go to next line
- Press <kbd>q</kbd> to close the buffer

### `* Stack dump *` buffer

Contains stack dump information.

- Press <kbd>SPC</kbd> to jump to gdscript file where stack frame points to
- Press <kbd>RET</kbd> to jump to gdscript file and to show `* Stack frame vars *`, `* Inspector *` buffers and a Debug Hydra
- Press <kbd>l</kbd> to display `* Stack frame vars *` buffer
- Press <kbd>p</kbd> to go to previous line
- Press <kbd>n</kbd> to go to next line
- Press <kbd>q</kbd> to close the buffer

### `* Breakpoints *` buffer

Contains list of all existing breakpoints.

- Press <kbd>SPC</kbd> to enable/disable all breakpoints
- Press <kbd>RET</kbd> to open gdscript file where on give breakpoint position
- Press <kbd>TAB</kbd> to display `* Stack dump *` buffer
- Press <kbd>D</kbd> to delete the breakpoint
- Press <kbd>q</kbd> to close the buffer
