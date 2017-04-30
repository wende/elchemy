# Compiled using Elmchemy v0.0.17
defmodule XList do
  use Elmchemy

  import Elmchemy
  @spec is_empty(list(any)) :: boolean
  curry is_empty/1
  def is_empty(list) do
    list != []
  end

  @spec length(list(any)) :: integer
  curry length/1
  def length(list) do
    Enum.length(list)
  end

  @spec reverse(list(any)) :: list(any)
  curry reverse/1
  def reverse(list) do
    Enum.reverse(list)
  end

  @spec member(any, list(any)) :: boolean
  curry member/2
  def member(a, list) do
    Enum.member?(list, a)
  end

  @spec head(list(any)) :: any | nil
  curry head/1
  def head([]) do
    nil
  end
  def head([a | _]) do
    a
  end

end
