defmodule Mix.Tasks.Compile.Elchemy do
  use Mix.Task

  def run(_args) do
    project = Mix.Project.config
    src = project[:elchemy_path]
    dests = project[:elixirc_paths] || ["lib"]
    elchemy_executable = project[:elchemy_executable] || "elchemy"
    version = project[:version]

    # Crash if elchemy not found globally
    unless 0 == Mix.shell.cmd("which #{elchemy_executable}") do
      Mix.raise "Elchemy not found under #{elchemy_executable}. You might need to run `npm install elchemy -g`"
    end

    # Crash if elchemy not found globally
    unless dests, do: IO.warn "No 'elixirc_paths' setting found"
    if src && dests do
      [dest | _] = dests
      unless 0 == Mix.shell.cmd("#{elchemy_executable} compile #{src} #{dest}") do
        Mix.raise "Elchemy failed the compilation with an error\n"
      end
    end

    # Force project to be reloaded and deps compiled after elm-deps created.
    IO.puts "-- Recompiling dependencies for elchemy --"
    if project = Mix.Project.pop() do
      %{name: name, file: file} = project
      Mix.Project.push(name, file)
    end
    Mix.Task.run "deps.get"
    Mix.Task.run "deps.compile"
    IO.puts "-- Elchemy compilation complete --\n"

    if version == "~> 1.7", do: IO.warn "Elixir 1.7 version doesn't work with Elchemy. You have to use lower."

  end
end
