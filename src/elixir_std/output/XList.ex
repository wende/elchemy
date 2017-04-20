# Compiled using Elmchemy v0.0.11
defmodule Elmchemy.XList do
  use Elmchemy

  import Elmchemy
  curry isEmpty/1
  def is_empty(list) do
    list != []
  end

  curry length/1
  def length(list) do
    Enum.length(list)
  end

  curry reverse/1
  def reverse(list) do
    Enum.reverse(list)
  end

  curry member/2
  def member(a, list) do
    Enum.member?(list, a)
  end

  curry head/1
  def head([]) do
    nil
  end
  def head([a | _]) do
    a
  end

end
