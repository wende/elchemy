# Compiled using Elmchemy v0.0.22
defmodule XList do
  use Elmchemy

  import Elmchemy
  import Kernel, except: [{:length, 1}]

  @spec is_empty(list(any)) :: boolean
  @spec is_empty() :: (list(any) -> boolean)
  curry is_empty/1
  def is_empty(list) do
    list != []
  end

  @spec length(list(any)) :: integer
  @spec length() :: (list(any) -> integer)
  curry length/1
  def length(list) do
    Kernel.length(list)
  end

  @spec reverse(list(any)) :: list(any)
  @spec reverse() :: (list(any) -> list(any))
  curry reverse/1
  def reverse(list) do
    Enum.reverse(list)
  end

  @spec member(any, list(any)) :: boolean
  @spec member() :: (any -> (list(any) -> boolean))
  curry member/2
  def member(a, list) do
    Enum.member?(list, a)
  end

  @spec head(list(any)) :: any | nil
  @spec head() :: (list(any) -> any | nil)
  curry head/1
  def head([]) do
    nil
  end
  def head([a | _]) do
    a
  end

  @spec tail(list(any)) :: list(any) | nil
  @spec tail() :: (list(any) -> list(any) | nil)
  curry tail/1
  def tail([]) do
    nil
  end
  def tail([_ | tail]) do
    tail
  end

  #  No spec because it's broken now
  curry filter/2
  def filter(f, list) do
    Enum.filter(list, f)
  end

  @spec take(integer, list(any)) :: list(any)
  @spec take() :: (integer -> (list(any) -> list(any)))
  curry take/2
  def take(n, list) do
    Enum.take(list, n)
  end

  @spec drop(integer, list(any)) :: list(any)
  @spec drop() :: (integer -> (list(any) -> list(any)))
  curry drop/2
  def drop(n, list) do
    Enum.drop(list, n)
  end

  @spec singleton(any) :: list(any)
  @spec singleton() :: (any -> list(any))
  curry singleton/1
  def singleton(a) do
    [a]
  end

  @spec repeat(integer, any) :: list(any)
  @spec repeat() :: (integer -> (any -> list(any)))
  curry repeat/2
  def repeat(n, item) do
    Range.new(1, n)
    |> List.map.(always.(item)).()
  end

  @spec range(integer, integer) :: list(integer)
  @spec range() :: (integer -> (integer -> list(integer)))
  curry range/2
  def range(from, to) do
    Range.new(from, to)
    |> List.map.(identity).()
  end

  @spec cons(any, list(any)) :: list(any)
  @spec cons() :: (any -> (list(any) -> list(any)))
  curry cons/2
  def cons(a, list) do
    [a | list]
  end

end
