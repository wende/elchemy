defmodule MyTest do
  def test() do
    for i <- 1..10 do
      name = to_string(Module) <> to_string(i) |> String.to_atom()
      defmodule name do
        def test(), do: 1
      end
    end
  end
end
