defp foobar() do
  if false do
    :foo
  else
    :bar |> foo
  end
end
