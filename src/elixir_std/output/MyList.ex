# Compiled using Elmchemy v0.0.7
defmodule MyList do
  use Elmchemy

  import Elmchemy
  @spec is_empty(list(any)) :: Bool.t
  def is_empty(list) do
    list != []
  end

  @spec length(list(any)) :: int
  def length(list) do
    Enum.length(list)
  end

  @spec reverse(list(any)) :: list(any)
  def reverse(list) do
    Enum.reverse(list)
  end

  @spec member(any, list(any)) :: Bool.t
  def member(a,list) do
    Enum.member?(list, a)
  end

  @spec head(list(any)) :: any | nil
  def head([]) do
    nil
  end
  def head([a | _]) do
    a
  end

end
