# Experimental Elixir Major Mode with Treesitter

Using treesitter for font-lock, indentation, imenu,
which-function-mode and navigation.

## Install

- Checkout the emacs `feature/tree-sitter` branch. 
- Checkout https://github.com/casouri/tree-sitter-module
- Run `[cloned casouri/tree-sitter-module]/tree-sitter-module/batch.sh`
- Add the following to your emacs config

```elisp
    (add-to-list 'treesit-extra-load-path "[cloned directory]/tree-sitter-module/dist/")
    
    (load "[cloned wkirschbaum/elixir-mode]/test-treesitter/elixir-mode.el")
    (load "[cloned wkirschbaum/elixir-mode]/heex-mode.el")
```

## TODO

    [ ] Handle Heex
    [ ] Fix Begin/End defun
    [ ] Improve Indentation
    [ ] Improve imenu
    [ ] Improve forward-sexp
    [ ] Figure out why sigil start breaks fonts
