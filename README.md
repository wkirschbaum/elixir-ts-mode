# Experimental Elixir and Heex Major Modes using Treesitter

Using treesitter for font-lock, indentation, imenu,
which-function-mode and navigation.

## Install

- Ensure you are using the latest emacs `emacs-29` branch or later. 
- Ensure you have tree-sitter installed
- Compile emacs with the --with-tree-sitter flag by running `./configure --with-tree-sitter`
- Checkout https://github.com/casouri/tree-sitter-module
- Run `[cloned casouri/tree-sitter-module]/tree-sitter-module/batch.sh`
- Add the following to your emacs config

```elisp
    (add-to-list 'treesit-extra-load-path "[cloned directory]/tree-sitter-module/dist/")
    
    (load "[cloned wkirschbaum/elixir-mode]/heex-ts-mode.el")
    (load "[cloned wkirschbaum/elixir-mode]/elixir-ts-mode.el")
```

## TODO

    [ ] Impliment forward-sexp
