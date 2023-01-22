(require 'ert)
(require 'ert-x)
(require 'treesit)

(ert-deftest elixir-ts-mode-test-indentation ()
  (skip-unless (and (treesit-ready-p 'elixir) (treesit-ready-p 'heex)))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest elixir-ts-mode-test-string-indentation ()
  (skip-unless (and (treesit-ready-p 'elixir) (treesit-ready-p 'heex)))
  (ert-test-erts-file (ert-resource-file "string-indent.erts")))

(provide 'elixir-ts-mode-tests)
;;; elixir-ts-mode-tests.el ends here
