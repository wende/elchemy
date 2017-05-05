defmodule Mix.Tasks.Compile.Elmchemy do
  use Mix.Task

  def run(_args) do
    project = Mix.Project.config
    src = project[:elmchemy_path]
    dests = project[:elixirc_paths] || ["lib"]

    unless src, do: IO.warn "No 'elmchemy_path' setting found"
    unless dests, do: IO.warn "No 'elixirc_paths' setting found"
    if src && dests do
      [dest | _] = dests
      Mix.shell.cmd "elmchemy compile #{src} #{dest}"
    end
  end
end
