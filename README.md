# Elixir and Heex Major Modes using tree-sitter

Using [tree-sitter](https://tree-sitter.github.io/tree-sitter/) for font-lock, indentation, imenu and navigation.

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

### Using with Eglot

```elisp
(require 'eglot)

(dolist (mode '(elixir-mode elixir-ts-mode heex-ts-mode))
    (add-to-list 'eglot-server-programs `(,mode . ("[elixir language server path]"))))

(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)
(add-hook 'heex-ts-mode-hook 'eglot-ensure)
```

### Using with lsp-mode

Ensure to add two additional hooks after elixir-mode ( refer to
https://emacs-lsp.github.io/lsp-mode/page/installation/ )

```
(elixir-mode    . lsp)
(elixir-ts-mode . lsp)
(heex-ts-mode   . lsp)
```

While [this change](https://github.com/emacs-lsp/lsp-mode/pull/3883)
has not been released, you can add the following so long:

```elisp
(require 'lsp-mode)

(setq lsp-language-id-configuration
      (append lsp-language-id-configuration
              '((elixir-ts-mode . "elixir")
                (heex-ts-mode . "elixir"))))
```

### Installing emacs-29 on Mac OS or Linux via Homebrew

```bash
brew install tree-sitter
brew install emacs-plus@29
```

## Development

Tree-sitter starter guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29

To test you can run `make test` which will download a batch script
from https://github.com/casouri/tree-sitter-module and compile
tree-sitter-elixir as well as tree-sitter-heex. 

Requirements:

- make
- git
- curl
- tree-sitter 0.20.7
