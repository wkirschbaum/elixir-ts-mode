# Elixir Major Mode using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/elixir-ts-mode-badge.svg)](https://melpa.org/#/elixir-ts-mode)
![CI](https://github.com/wkirschbaum/elixir-ts-mode/actions/workflows/ci.yml/badge.svg)

> [!NOTE]  
> This package is a backport from Emacs core for the use with Emacs 29.1. Please report bugs or submit patches to the Emacs directly: https://www.gnu.org/software/emacs/manual/html_node/emacs/Bugs.html.

For an implementation without tree-sitter support please have a
look at: https://github.com/elixir-editors/emacs-elixir

This package is compatible with and was tested against the tree-sitter grammar
for Elixir found at https://github.com/elixir-lang/tree-sitter-elixir.

## Installing

Emacs 29.1 or above with tree-sitter support is required. 

Tree-sitter starter guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29

You can install the tree-sitter Elixir and HEEx grammars by running: `M-x elixir-ts-install-grammar`.

### Using MELPA and use-package

```elisp
(use-package elixir-ts-mode
    :ensure t)
```

### From source

- Clone this repository
- Add the following to your emacs config

It is also necessary to clone 
[heex-ts-mode](https://github.com/wkirschbaum/heex-ts-mode) and
load the heex-ts-mode.el file before loading elixir-ts-mode.el:

```elisp
(load "[cloned wkirschbaum/heex-ts-mode]/heex-ts-mode.el")
(load "[cloned wkirschbaum/elixir-ts-mode]/elixir-ts-mode.el")
```

### Installing emacs-29 on Mac OS or Linux via Homebrew

This uses [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus). Note
that we grab its "tap" so Homebrew will know about its formula.

```bash
brew install tree-sitter
brew tap d12frosted/emacs-plus
brew install emacs-plus@29
```

### Troubleshooting

If you get the following warning:

```
⛔ Warning (treesit): Cannot activate tree-sitter, because tree-sitter
library is not compiled with Emacs [2 times]
```

Then you do not have tree-sitter support for your emacs installation.

If you get the following warnings:
```
⛔ Warning (treesit): Cannot activate tree-sitter, because language grammar for heex is unavailable (not-found): (libtree-sitter-heex libtree-sitter-heex.so) No such file or directory
⛔ Warning (treesit): Cannot activate tree-sitter, because language grammar for elixir is unavailable (not-found): (libtree-sitter-elixir libtree-sitter-elixir.so) No such file or directory
```

then the grammar files are not properly installed on your system.

## Development

To test you can run `make test` which will download a batch script
from https://github.com/casouri/tree-sitter-module and compile
tree-sitter-elixir as well as tree-sitter-heex. 

Requirements:

- tree-sitter
- make
- gcc
- git
- curl


Please make sure you run `M-x byte-compile-file` against the updated
file(s) with an emacs version --with-tree-sitter=no to ensure it still
works for non tree-sitter users. 
