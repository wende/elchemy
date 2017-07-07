defmodule ElchemyTest do
  use ExUnit.Case
  use Elchemy
  doctest Hello

  test "Hello" do
    assert Hello.hello() == "world!"
  end

end
