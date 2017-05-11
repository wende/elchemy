defmodule ElmchemyTest do
  use ExUnit.Case

  doctest Hello

  test "Hello" do
    assert Hello.hello() == "world!"
  end

end
