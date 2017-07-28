defmodule Mix.Tasks.Elchemy.New do

  alias Elchemy.Project
  alias Elchemy.NewProject

  @legal_options [
    location: :string
  ]

  @moduledoc """
  Task for generation of Elchemy projects.

  Switches:
    --location  -  specify location for generation

  Sample usage:

    mix elchemy.new porject_name --location folder
  """

  def run([]) do
    Mix.Tasks.Help.run(["elchemy.new"])
  end

  def run(argv) do
    [name | opts] = argv
    project = Project.new(name)
    project = apply_opts(project, opts)
    Elchemy.Generator.copy_from(project, NewProject, :new)
    Mix.shell.info("Project generated successfully")
  end

  def apply_opts(project, opts) do
    case OptionParser.parse(opts, switches: @legal_options) do
      {[location: location], _, _} -> %{project | project_path: location}
      _ -> project
    end
  end

end
