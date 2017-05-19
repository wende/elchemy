defmodule ElmchemyTest do
  use ExUnit.Case
  use Elmchemy
  doctest Hello

  test "Hello" do
    assert Hello.hello() == "world!"
  end

end
