
defmodule ElmchemyHack do


  defp get_definitions(content) do
    ~r/def (\w+)/
    |> Regex.scan(content)
  end

  defmacro __before_compile__(env) do
    IO.inspect env
    IO.inspect __CALLER__.module

    File.read!(env.file)
    |> IO.inspect
    |> get_definitions
    |> IO.inspect

    quote do
        def buagam, do: 2
    end
  end


end

defmodule Elmchemy do

  defmacro __using__(_) do
    quote do
      require Elmchemy
      require Elmchemy.Glue

      import Elmchemy
      import Elmchemy.Glue
    end
  end
  @moduledoc """
  Documentation for Elmchemy.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Elmchemy.hello
      :world

  """
  # @before_compile ElmchemyHack

  def hello do
    :world
  end
end
