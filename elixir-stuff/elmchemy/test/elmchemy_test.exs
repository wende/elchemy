defmodule Elmchemy.Test do
  require Elmchemy.Glue
  import Elmchemy.Glue


  defcurry add(a, b) do
    notadd().(a).(b)
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

  test "Currying" do
    assert Elmchemy.Test.add().(1).(2) == 3
    assert Elmchemy.Test.add(1, 2) == 3
    assert Elmchemy.Test.add2(1, 2) == 3
    assert Elmchemy.Test.add2().(1).(2) == 3
  end
end
