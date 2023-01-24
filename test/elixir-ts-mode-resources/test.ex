"foo"
|> case do
  "foobar" ->
    # foobar
    raise "an error"

  other -> :other
end


@type something() :: [
  "foo",
  "bar"
]

@type something() :: %{
  one: one(),
  three: two()
}

def something(%{
  one: :one,
  two: :two
}) do
  {:ok, "done"}
end
