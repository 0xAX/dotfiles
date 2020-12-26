[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/elixir-lang/emacs-elixir.svg?branch=master)](https://travis-ci.org/elixir-lang/emacs-elixir)
[![MELPA Stable](http://stable.melpa.org/packages/elixir-mode-badge.svg)](http://stable.melpa.org/#/elixir-mode)

# Elixir Mode

Provides font-locking, indentation and navigation support for the
[Elixir programming language.](http://elixir-lang.org/)

- [Installation](#installation)
  - [Via package.el](#via-packageel)
  - [Via el-get](#via-el-get)
  - [Manual](#manual)
- [Usage](#usage)
  - [Interactive Commands](#interactive-commands)
  - [Configuration](#configuration)
  - [Hooks](#hooks)
  - [Keymapping](#keymapping)
- [Notes](#notes)
- [Elixir Tooling Integration](#elixir-tooling-integration)
- [History](#history)
- [Contributing](#contributing)

## Installation

### Via package.el

`package.el` is the built-in package manager in Emacs.

`Elixir-Mode` is available on the two major community maintained repositories -
[MELPA STABLE](melpa-stable.milkbox.net) and [MELPA](http://melpa.milkbox.net).

You can install `Elixir-Mode` with the following command:

<kbd>M-x package-install [RET] elixir-mode [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining `Elixir-Mode`, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

With the most recent builds of Emacs, you can pin `Elixir-Mode` to always
use MELPA Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(elixir-mode . "melpa-stable") t)
```

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs. If you're an el-get
user just do <kbd>M-x el-get-install [RET] elixir-mode [RET]</kbd>.

### Manual

You can install `Elixir-Mode` manually by placing `Elixir-Mode` on your `load-path` and
`require` ing it. Many people favour the folder `~/.emacs.d/vendor`.

```el
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'elixir-mode)
```

## Usage

### Interactive Commands

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>elixir-mode</code></td>
        <td>Switches to elixir-mode.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-compile-file</code></td>
        <td>Compile Elixir files. Works fine on <code>*.exs</code> files, too, if needed.</td>
     <tr>
        <td><code>elixir-cos-mode</code></td>
        <td>Applies compile-on-save minor mode.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-iex</code></td>
        <td>
            Launch <code>iex</code> inside Emacs. Use <code>C-u</code>
            <a href="http://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html">univesal-argument</a>
            to run <code>iex</code> with some additional arguments.
        </td>
    </tr>
    <tr>
        <td><code>elixir-mode-eval-on-region</code></td>
        <td>Evaluates the Elixir code on the marked region.
            This is bound to <code>C-c ,r</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-eval-on-current-line</code></td>
        <td>Evaluates the Elixir code on the current line.
            This is bound to <code>C-c ,c</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-eval-on-current-buffer</code></td>
        <td>Evaluates the Elixir code in the current buffer.
            This is bound to <code>C-c ,b</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-string-to-quoted-on-region</code></td>
        <td>Get the representation of the expression on the marked region.
            This is bound to <code>C-c ,a</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-string-to-quoted-on-current-line</code></td>
        <td>Get the representation of the expression on the current line.
            This is bound to <code>C-c ,l</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-opengithub</code></td>
        <td>Open the GitHub page for Elixir.</td>
    </tr>
    </tr>
    <tr>
        <td><code>elixir-mode-open-elixir-home</code></td>
        <td>Go to Elixir README in the browser.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-open-docs-master</code></td>
        <td>Open the Elixir documentation for the master.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-open-docs-stable</code></td>
        <td>Open the Elixir documentation for the latest stable release.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-show-version</code></td>
        <td>Print version info for elixir-mode.</td>
    </tr>
</table>

### Configuration

Any file that matches the glob `*.ex[s]` or `*.elixir` is
automatically opened in elixir-mode, but you can change this
functionality easily.

```lisp
;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
```

Custom variables for elixir-mode.

<table>
    <tr>
        <th>Variable</th>
        <th>Default</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>elixir-compiler-command (string)</code></td>
        <td><code>"elixirc"</code></td>
        <td>Command to compile Elixir code.</td>
    </tr>
    <tr>
        <td><code>elixir-iex-command (string)</code></td>
        <td><code>"iex"</code></td>
        <td>Command to start an interactive REPL in <code>IEX</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-cygwin-paths (boolean)</code></td>
        <td><code>t</code></td>
        <td>Should Cygwin paths be used on Windows?</td>
    </tr>
    <tr>
        <td><code>elixir-mode-cygwin-prefix (string)</code></td>
        <td><code>"/cygdrive/C"</code></td>
        <td>The prefix for Cygwin-style paths.</td>
    </tr>
</table>

### Hooks

Hooks can be used to add functionality to elixir-mode. This example
adds compile on save.

```lisp
(defun elixir-mode-compile-on-save ()
  "Elixir mode compile files on save."
	(and (file-exists (buffer-file-name))
	     (file-exists (elixir-mode-compiled-file-name))
			 (elixir-cos-mode t)))
(add-hook 'elixir-mode-hook 'elixir-mode-compile-on-save)
```

### Keymapping

Keymaps can be added to the `elixir-mode-map` variable.

## Notes

If you want to use `ruby-end-mode` for a more comfortable editing
experience, you can add the following to your `elixir-mode-hook`:

```lisp
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))
```

## Elixir Tooling Integration

If you looking for elixir tooling integration for emacs, check: [alchemist.el](https://github.com/tonini/alchemist.el)

## History

This mode is based on the
[Emacs mode by secondplanet](https://github.com/secondplanet/elixir-mode).

## Contributing

Please read [CONTRIBUTING.md](https://github.com/elixir-lang/emacs-elixir/blob/master/CONTRIBUTING.md) for guidelines on how to contribute to this project.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
