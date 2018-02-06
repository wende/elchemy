defmodule Elchemy.GlueTest do
  use ExUnit.Case

  defmodule Mock do
    require Elchemy.Glue
    import Elchemy.Glue

    def test(x1,x2,x3,x4), do: [x1,x2,x3,x4]
    defswap swapped_test, to: test/4
    defreverse reverse_test, to: test/4

    curry myfun/3, lambdas: [{0,2}, {2,3}]
    def myfun(a, _b, c) do
      {a.(1).(2), c.(2).(3).(4)}
    end
  end

  test "Mock works" do
    assert Mock.test(1,2,3,4) == [1,2,3,4]
  end

  test "Reverse arguments" do
    assert Mock.reverse_test(1,2,3,4) == [4,3,2,1]
  end

  test "Swapped arguments" do
    assert Mock.swapped_test(1,2,3,4) == [4,1,2,3 ]
  end

  test "Can unlambdify passed functions" do
    duo = fn a, b -> a + b end
    trio = fn a, b, c -> {a, b, c} end
    assert Mock.myfun(duo, 0, trio) == {3, {2,3,4}}
  end

  test "Can define recursive function" do
    import Elchemy.Glue

    fun = rec fun, fn
      0 -> 0
      a -> a + fun.(a - 1)
    end

    assert fun.(3) == 6
  end

  test "Can define mutually recursive functions" do
    import Elchemy.Glue

    fun = rec fun, fn
      :x ->
        {x, y} = {&fun.(:x).(&1), &fun.(:y).(&1)}
        {_, _} = {x, y}
        fn a -> y.(a) - 1 end
      :y ->
        {x, y} = {&fun.(:x).(&1), &fun.(:y).(&1)}
        {_, _} = {x, y}
        fn a -> a end
    end
    {x, y} = {&fun.(:x).(&1), &fun.(:y).(&1)}

    assert x.(10) == 9
    assert y.(10) == 10
  end

  test "Can use DSL syntax to define mutually recursive functions" do
    import Elchemy.Glue

    {x, y} = let [
      x: fn a -> y.(a) - 1 end,
      y: fn a -> a end
    ]

    assert x.(10) == 9
    assert y.(10) == 10
  end

  test "Accessors and divisors" do
    assert Elchemy.Glue.update_in_([:x]).(fn x -> x + 1 end).(%{x: 1, y: 2}) == %{x: 2, y: 2}
    assert Elchemy.Glue.update_in_([:x, :y]).(fn x -> x + 1 end).(%{x: %{y: 10}, y: 2}) == %{x: %{y: 11}, y: 2}

    assert Elchemy.Glue.put_in_([:x, :y]).(20).(%{x: %{y: 10}, y: 2}) == %{x: %{y: 20}, y: 2}

    assert Elchemy.Glue.get_in_([:x, :y]).(%{x: %{y: 10}, y: 2}) == 10
  end
end
