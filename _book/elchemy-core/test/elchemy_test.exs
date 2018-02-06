defmodule Helpers do
  defmacro gen_tests(tests) do
    for mod <- tests do
      quote do
        typetest unquote(mod)
        doctest unquote(mod)
      end
    end
  end
end

defmodule ElchemyTest do
  use ExUnit.Case
  use Elchemy
  import Helpers

  doctest Elchemy

  gen_tests([
    Elchemy.XBasics,
    Elchemy.XList,
    Elchemy.XString,
    Elchemy.XMaybe,
    Elchemy.XChar,
    Elchemy.XResult,
    Elchemy.XTuple,
    Elchemy.XDict,
    Elchemy.XBitwise,
    Elchemy.XSet,
    Elchemy.XRegex
  ])
  # No typetest for Debug because it loops forever on type resolution
  doctest Elchemy.XDebug


  test "Full numbers division" do
    assert Elchemy.XBasics./(10, 0) == :infinity
    assert Elchemy.XBasics./(0, 0) == :nan
    assert Elchemy.XBasics./(10, 10) == 1

    assert Elchemy.XBasics.div(10, 0) == 0
  end
end
