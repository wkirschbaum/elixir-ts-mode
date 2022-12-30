# Elixir and Heex Major Modes using tree-sitter

Using tree-sitter for font-lock, indentation, imenu and navigation.

This must not be confused with the MELPA tree-sitter package as it
uses the built-in emacs treesit package.

For an implementation without tree-sitter support please have a
look at: https://github.com/elixir-editors/emacs-elixir

## Install

- Ensure you have tree-sitter 0.20.7 installed ( tree-sitter --version )
- Ensure you are using the latest `emacs-29` or `master` branch.
- You have to configure and compile emacs after you install tree-sitter
- Clone this repository
- Run `make dist` ( Please review the Makefile and download link(s) before
  running the make command and DO NOT run it as root or with sudo )
- Add the following to your emacs config

```elisp
    (when (boundp 'treesit-extra-load-path)
      (add-to-list 'treesit-extra-load-path "[cloned wkirschbaum/elixir-ts-mode]/dist/")
      (load "[cloned wkirschbaum/elixir-ts-mode]/heex-ts-mode.el")
      (load "[cloned wkirschbaum/elixir-ts-mode]/elixir-ts-mode.el"))
```

## Developement

Tree-sitter starter guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29

To test you can run `make test` which will download a batch script
from https://github.com/casouri/tree-sitter-module and compile
tree-sitter-elixir as well as tree-sitter-heex. 

Requirements:

- make
- git
- curl
- tree-sitter 0.20.7

## TODO

    [ ] Impliment forward-sexp
    [ ] standardise font-lock settings
