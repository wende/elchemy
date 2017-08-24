defmodule ElchemyExTest do
  use ExUnit.Case
  doctest ElchemyEx

  test "greets the world" do
    assert ElchemyEx.hello() == :world
  end
end
