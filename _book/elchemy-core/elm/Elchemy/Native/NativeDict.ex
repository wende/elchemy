defmodule Native.Dict do
  use Elchemy
  require Logger
  @spec empty :: %{}
  def empty, do: %{}

  @spec get(any, %{}) :: any
  def get(key, map) do
    case Map.get(map, key) do
      nil -> nil
      val -> {val}
    end
  end

  @spec foldr((any, any, any -> any), any, %{}) :: any
  def foldr(f, start, map) do
    map
    |> Map.to_list
    |> List.foldr(start, fn {k, v}, acc -> f.(k, v, acc) end)
  end

  @spec foldl((any, any, any -> any), any, %{}) :: any
  def foldl(f, start, map) do
    map
    |> Map.to_list
    |> List.foldl(start, fn {k, v}, acc -> f.(k, v, acc) end)
  end

  @spec map((any, any -> any), %{}) :: %{}
  defswap map, to: Enum.map/2

  @spec update(any, ({any} | nil -> {any} | nil), %{}) :: %{}
  def update(key, f, dict) do
    case dict[key] do
      nil -> f.(nil)
      value -> f.({value})
    end
    |> case do
       nil -> Map.put(dict, key, nil)
       {value} -> Map.put(dict, key, value)
     end
  end

  @spec size(%{}) :: integer
  def size(map), do: Enum.count(map)

  @spec remove(key :: any, %{}) :: %{}
  defswap remove, to: Map.delete/2

  @spec insert(key :: any, value :: any, %{}) :: %{}
  defswap insert, to: Map.put/3

  @spec merge((any, any, any -> any),
              (any, any, any, any -> any),
              (any, any, any -> any),
              %{},
              %{},
              any) :: any
  def merge(when_a, when_both, when_b, a, b, acc) do
    Map.keys(a) ++ Map.keys(b)
    |> MapSet.new
    |> Enum.reduce(acc, fn key, sofar ->
      case {a[key], b[key]} do
        {value, nil} -> when_a.(key, value, sofar)
        {nil, value} -> when_b.(key, value, sofar)
        {value_a, value_b} -> when_both.(key, value_a, value_b, sofar)
      end

    end)
  end


end
