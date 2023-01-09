# Elixir and Heex Major Modes using tree-sitter

Using [tree-sitter](https://tree-sitter.github.io/tree-sitter/) for font-lock, indentation, imenu and navigation.

For an implementation without tree-sitter support please have a
look at: https://github.com/elixir-editors/emacs-elixir

You must use either `elixir-mode` mode or `elixir-ts-mode` you can't
use both at the same time as both will load the first mode on the
`auto-mode-alist`.

## Installing

- Ensure you have tree-sitter 0.20.7 installed ( tree-sitter --version )
- Ensure you are using the latest `emacs-29` or `master` branch.
- You have to configure and compile emacs after you install tree-sitter

### Using MELPA and use-package

```elisp
(use-package elixir-ts-mode
    :ensure t)
```

It might make navigation simpler for modules when setting subword-mode

```elisp
(use-package elixir-ts-mode
    :ensure t
    :config
    (global-subword-mode t))
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

The packages are in different repositories to make it easier for MELPA
package management.


## Installing Language Grammars

Please confirm if you your system distribution has language grammars
available before using the below methods. The
`elixir-ts-install-grammar` and `treesit-install-language-grammar`
functions should only be used as a fallback if you can not obtain
tree-sitter-elixir or tree-sitter-heex from your system distribution.

You can install both tree-sitter-elixir and tree-sitter-heex grammars
by running `M-x elixir-ts-install-grammar` from within emacs. You can
also run `M-x treesit-install-language-grammar` to do so individually,
but you need to either specify the recipe or set the language source
like below first. You need to have git, as well as a c and c++
compiler installed for these functions to work. Ensure that you
confirm that you are happy to download and install these external
programs before using them.

If you prefer other grammar repositories for elixir and heex you can
set `treesit-language-source-alist` in your emacs config like this:

```elisp
(append treesit-language-source-alist
  '((elixir . ("https://[grammar-repo].git"))
    (heex . ("https://[grammar-repo].git"))))
```

`C-h v treesit-language-source-alist` for more info.

View the 

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

- tree-sitter
- make
- gcc
- git
- curl


Please make sure you run `M-x byte-compile-file` against the updated
file(s) with an emacs version --without-tree-sitter to ensure it still
works for non tree-sitter users. 
