defmodule Foo do

  defmodule Zar do
    def zoo(one) when one == "one" do
      "bar"
    end

  end


  def bar(one, two) do
    "bar"
  end

  def last(one) do

  end

  def bar(one, two) do
    "bar"
  end

  def foo(one, two) do
    def bar(one, two) do
      "bar"
    end
  end

  defmacro Bar do

    def bar() do

    end
  end
end
