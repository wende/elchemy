defmodule Elmchemy.Test do
  require Elmchemy.Glue
  import Elmchemy.Glue

  curry +/2

  defcurry add(a, b) do
    a + b
  end

  defcurryp notadd(a, b) do
    a + b
  end

  curry add2/2
  def add2(a, b), do: a + b

end

defmodule ElmchemyTest do
  use ExUnit.Case
  doctest Elmchemy
  import Elmchemy.Test

  test "Currying" do
    assert add().(1).(2) == 3
    assert add(1, 2) == 3
    assert add2(1, 2) == 3
    assert add2().(1).(2) == 3
    #assert Enum.map([1,2,3], +) == 6
  end
end
