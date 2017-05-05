# Compiled using Elmchemy v0.0.22
defmodule XTuple do
  use Elmchemy

  @spec first({any, any}) :: any
  @spec first() :: ({any, any} -> any)
  curry first/1
  def first({fst, _}) do
    fst
  end

  @spec second({any, any}) :: any
  @spec second() :: ({any, any} -> any)
  curry second/1
  def second({_, snd}) do
    snd
  end

  curry map_first/2
  def map_first(fn, {fst, snd}) do
    {fn.(fst), snd}
  end

  curry map_second/2
  def map_second(fn, {fst, snd}) do
    {fst, fn.(snd)}
  end

end
