defmodule Mix.Tasks.Elchemy.New do

  alias Elchemy.Project
  alias Elchemy.NewProject

  @legal_options [
    location: :string
  ]

  @moduledoc """
  Here will be help
  """

  def run(argv) do
    [name | opts] = argv
    project = Project.new(name)
    project = apply_opts(project, opts)
    Elchemy.Generator.copy_from(project, NewProject, :new)
  end

  def apply_opts(project, opts) do
    case OptionParser.parse(opts, switches: @legal_options) do
      {[location: location], _, _} -> %{project | project_path: location}
      _ -> project
    end
  end

end
