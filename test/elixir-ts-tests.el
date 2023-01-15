(require 'ert)
(require 'elixir-ts-mode)
(require 'heex-ts-mode)

(ert-deftest elixir-ts-mode-indentation ()
  "Test module indentation."
  (skip-unless (treesit-language-available-p 'elixir))
  (let ((original "defmodule Foo do
def bar(), do: \"bar\"
     end")
        (expected "defmodule Foo do
  def bar(), do: \"bar\"
end"))
  (with-temp-buffer
    (progn
      (insert original)
      (elixir-ts-mode)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) expected))))))
