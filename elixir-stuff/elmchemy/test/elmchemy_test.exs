defmodule Elmchemy.Test do
  require Elmchemy.Glue
  import Elmchemy.Glue

  defcurry add(a, b) do
    a + b
  end

  defcurryp notadd(a, b) do
    a + b
  end

  curry add2/2
  def add2(a, b), do: a + b

  curry +/2

end

defmodule ElmchemyTest do
  use ExUnit.Case
  doctest Elmchemy
  doctest Elmchemy.XBasics
  doctest Elmchemy.XList
  doctest Elmchemy.XString
  doctest Elmchemy.XMaybe
  doctest Elmchemy.XChar
  doctest Elmchemy.XResult
  doctest Elmchemy.XTuple

  import Elmchemy.Test


  test "Currying" do
    assert add().(1).(2) == 3
    assert add(1, 2) == 3
    assert add2(1, 2) == 3
    assert add2().(1).(2) == 3
    assert Elmchemy.Test.+().(1).(2) == 3
    assert (&+/0).().(1).(2) == 3
    assert Enum.map([1,2,3], (&+/0).().(1)) == [2,3,4]
  end
end
