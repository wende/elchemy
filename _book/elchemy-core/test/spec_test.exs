defmodule Elchemy.SpecTest do
  use ExUnit.Case

  alias Elchemy.Spec
  test "Can find spec" do
    assert Spec.find(Enum, :at, 3)
  end

  test "Can compare two same specs" do
    at = Spec.find(Enum, :at, 3)
    assert Spec.compare(at, at, Enum)
  end

  test "Can print specs as elm type " do
    gen_spec = &(Spec.find(&1, &2, &3) |> Spec.gen_elm(&1))

    assert gen_spec.(Enum, :map, 2) ==
      "map : term -> (element -> any) -> List any"
    assert gen_spec.(Enum, :at, 3) ==
      "at : term -> Int -> default -> element | default"
  end

  test "Can unify two different specs" do
    # They are just the same
    assert :ok = Spec.compare(
      Spec.find(List, :foldl, 3),
      Spec.find(List, :foldr, 3),
      List)
    # Any == Any so they are same
    assert :ok = Spec.compare(
      Spec.find(List, :delete_at, 2),
      Spec.find(List, :delete, 2),
      List)
    # Different arities
    assert {:error, _} = Spec.compare(
      Spec.find(List, :pop_at, 3),
      Spec.find(List, :delete_at, 2),
      List)
    # User types and remote types
    assert :ok = Spec.compare(
      Spec.find(List, :foldr, 3),
      Spec.find(Enum, :reduce, 3),
      List, Enum)
  end

  test "Can print wrong specs" do
    assert_raise(Elchemy.SpecError, fn ->
      Spec.compare!(
        Spec.find(List, :flatten, 1),
        {List, :first, 1},
        Enum, List)
    end)
  end
  # defmodule TypeTest do
  #   use Elchemy
  #
  #   @spec myfun1(integer, integer, integer) :: integer
  #   verify as: Enum.at/3
  #   defdelegate myfun1(a, b, c), to: Enum
  #
  #   @spec myfun1(list(), float, integer) :: integer
  #   verify as: Enum.at/3
  #   defdelegate myfun2(a, b, c), to: Enum
  #
  #   @spec myfun1(list(), float, integer) :: integer
  #   verify as: Enum.at/3
  #   defdelegate myfun3(a, b, c), to: Enum
  # end
  test "Test verify type" do

  end
end
