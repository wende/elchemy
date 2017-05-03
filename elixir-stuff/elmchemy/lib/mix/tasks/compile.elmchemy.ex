defmodule Mix.Tasks.Compile.Elmchemy do
  use Mix.Task

  def run(_args) do
    project = Mix.Project.config
    src = project[:elmchemy_path]
    dests = project[:elixirc_paths]

    Enum.each(dests, fn dest ->
      Mix.shell.cmd "elmchemy aggregate #{src} #{dest}"
    end)
  end
end
