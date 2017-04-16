defmodule ElmchemyTest do
  use ExUnit.Case
  doctest Elmchemy

  test "the truth" do
    assert Elmchemy.Test.add().(1).(2) == Elmchemy.Test.add(1, 2)
  end
end
