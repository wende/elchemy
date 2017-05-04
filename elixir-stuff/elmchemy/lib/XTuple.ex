# Compiled using Elmchemy v0.0.22
defmodule XTuple do
  use Elmchemy

  @spec first({any, any}) :: any
  curry first/1
  def first({fst, _}) do
    fst
  end

  @spec second({any, any}) :: any
  curry second/1
  def second({_, snd}) do
    snd
  end

  @spec map_first(any) :: any{any, any}) :: {any, any}
  curry map_first/2
  def map_first(fn, {fst, snd}) do
    {fn.(fst), snd}
  end

  @spec map_second(any) :: any{any, any}) :: {any, any}
  curry map_second/2
  def map_second(fn, {fst, snd}) do
    {fst, fn.(snd)}
  end

end
