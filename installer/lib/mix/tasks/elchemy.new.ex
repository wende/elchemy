defmodule Mix.Tasks.Elchemy.New do

  alias Elchemy.Project
  alias Elchemy.NewProject

  @moduledoc """
  Here will be help
  """

  def run(argv) do
    [name | _opts] = argv
    project = Project.new(name)
    Elchemy.Generator.copy_from(project, NewProject, :new)
  end

end
