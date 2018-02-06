defmodule Native.Regex do

  @type how_many :: :all | {:at_most, integer}
  @type regex :: {:regex, String.t, String.t}
  @type match :: %{
    index: integer,
    match: String.t,
    submatches: list({String.t} | nil),
    number: integer()
  }

  @spec escape(String.t) :: String.t
  defdelegate escape(string), to: Regex

  @spec regex(String.t) :: regex
  def regex(string), do: {:regex, string, ""}

  @spec case_insensitive(regex) :: regex
  def case_insensitive({:regex, string, _}), do: {:regex, string, "i"}

  @spec contains(regex, String.t) :: boolean
  def contains(regex, string), do: string =~ r(regex)

  @spec find(how_many, regex, String.t) :: list(match)
  def find(how_many, regex, string) do
    regex = r(regex)
    matches = Regex.scan(regex, string)
    indices = Regex.scan(regex, string, return: :index)

    for {{[m | sm], [{i, _lenght} | _]}, number} <- Enum.zip(matches, indices) |> Enum.with_index(1) do
      %{
        index: i,
        match: m,
        submatches: sm |> Enum.map(fn a -> {a} end),
        number: number
      }
    end |> times(how_many)
  end

  @spec replace(how_many, regex, (match -> String.t), String.t) :: String.t
  def replace(:all, regex, f, string) do
    Regex.replace(r(regex), string, f)
  end

  @spec split(how_many, regex, String.t) :: list(String.t)
  def split(:all, regex, string) do
    Regex.split(r(regex), string)
  end
  def split({:at_most, x}, regex = {:regex, s, _}, string) do
    split(:all, regex, string)
    |> Enum.split(x)
    |> case do
      {x, y} -> x ++ [(y |> Enum.join(s))]
    end
  end

  defp times(a, :all), do: a
  defp times(a, {:at_most, x}), do: a |> Enum.take(x)
  defp r({:regex, string, opts}), do: Regex.compile!(string, opts)
end
