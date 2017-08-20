defmodule Mix.Tasks.Compile.Elchemy do
  use Mix.Task

  def run(_args) do
    project = Mix.Project.config
    src = project[:elchemy_path]
    dests = project[:elixirc_paths] || ["lib"]

    #unless src, do: IO.warn "No 'elchemy_path' setting found"
    unless dests, do: IO.warn "No 'elixirc_paths' setting found"
    if src && dests do
      [dest | _] = dests
      unless 0 == Mix.shell.cmd("elchemy compile #{src} #{dest}") do
        Mix.raise "Compilation error"
      end
    end
  end
end
