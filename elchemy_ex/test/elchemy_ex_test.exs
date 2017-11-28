defmodule ElchemyExTest do
  use ExUnit.Case

  test "Parsing works" do
    assert Ast.parse("module A exposing (..)\na = 1") != []
  end
end
