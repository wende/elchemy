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

  curry map_first/2
  def map_first(f, {fst, snd}) do
    {f.(fst), snd}
  end

  curry map_second/2
  def map_second(f, {fst, snd}) do
    {fst, f.(snd)}
  end

end
