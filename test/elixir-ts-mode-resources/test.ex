with one <- one(),
     two <- two(),
     {:ok, value} <- get_value(one, two) do
  {:ok, value}
else
  {:error, {"Message" => message}} ->
    {:error, message}
end
