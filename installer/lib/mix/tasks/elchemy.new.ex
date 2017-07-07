defmodule Mix.Tasks.Elmchemy.New do

  @moduledoc """
  Here will be help
  """

  def run(argv) do
    [name | _opts] = argv
    IO.inspect name
  end

end
