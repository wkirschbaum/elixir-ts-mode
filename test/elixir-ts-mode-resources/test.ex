@spec foo_bar(
        t,
        (foo -> any),
        (() -> any) | (foo, foo -> boolean) | module()
      ) :: any
      when foo: any
def foo(one, fun, other)

barfoo(
  :bar,
  :zar,
  :bar
)

# (goto-char (treesit-node-start (treesit-parent-until (treesit-node-parent (treesit-node-at (point))) (lambda (node) (equal (treesit-node-type node) "call")))))

with one <- one(),
     two <- two(),
     {:ok, value} <- get_value(one, two) do
  {:ok, value}
else
  {:error, {"Message" => message} ->
    {:error, message}
end

[1, 2]
|> Enum.map(
  one,
  two
)

"foo" <>
  "bar" <>
    "zoo" <>
      "bar"
