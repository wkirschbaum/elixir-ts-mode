(require 'ert)
(require 'elixir-ts-mode)
(require 'heex-ts-mode)

(ert-deftest elixir-ts-mode-indentation ()
  "Test module indentation."
  (skip-unless (and (treesit-language-available-p 'elixir)
                    (treesit-language-available-p 'heex)))
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

(ert-deftest elixir-ts-mode-indentation-map ()
  "Test module indentation."
  (skip-unless (and (treesit-language-available-p 'elixir)
                    (treesit-language-available-p 'heex)))
  (let ((original "map = %{
\"a\" => 1,
\"b\" => 2
     }")
        (expected "map = %{
  \"a\" => 1,
  \"b\" => 2
}"))
  (with-temp-buffer
    (progn
      (insert original)
      (elixir-ts-mode)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) expected))))))
