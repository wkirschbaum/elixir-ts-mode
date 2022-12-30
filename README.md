# Elixir and Heex Major Modes using Treesitter

Using treesitter for font-lock, indentation, imenu and navigation.

## Install

- Ensure you have tree-sitter 0.20.7 installed ( tree-sitter --version )
- Ensure you are using the latest `emacs-29` or `master` branch.
- Clone this repository
- Run `make dist` ( Please review the script and download links before
  running this )
- Add the following to your emacs config

```elisp
    (when (boundp 'treesit-extra-load-path)
      (add-to-list 'treesit-extra-load-path "[cloned wkirschbaum/elixir-mode]/dist/")
      (load "[cloned wkirschbaum/elixir-mode]/heex-ts-mode.el")
      (load "[cloned wkirschbaum/elixir-mode]/elixir-ts-mode.el"))
```

## Developement

To test you can run `make test` which will download a batch script
from https://github.com/casouri/tree-sitter-module and compile
tree-sitter-elixir as well as tree-sitter-heex. 

Requirements:

- make
- git
- wget
- tree-sitter 0.20.7

## TODO

    [ ] Impliment forward-sexp
    [ ] standardise font-lock settings
