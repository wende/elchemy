defmodule Elchemy.Project do
  defstruct app_name: "", app_module: nil, project_path: "./", binding: []

  alias __MODULE__

  def new(app_name) do
    mod = Macro.camelize(app_name)
    %__MODULE__{app_name: app_name, app_module: mod, project_path: "#{app_name}", binding: [
                   app_name: app_name,
                   app_module: mod
                 ]}
  end

  def join_path(%Project{} = project, location, path) do

    project
    |> Map.fetch!(:"#{location}_path")
    |> Path.join(path)
    |> expand_path_with_bindings(project)
  end

  defp expand_path_with_bindings(path, %Project{} = project) do
    Regex.replace(recompile(~r/:[a-zA-Z0-9_]+/), path, fn ":" <> key, _ ->
      project |> Map.fetch!(:"#{key}") |> to_string()
    end)
  end

  def recompile(regex) do
    if Code.ensure_loaded?(Regex) and function_exported?(Regex, :recompile!, 1) do
      apply(Regex, :recompile!, [regex])
    else
      regex
    end
  end
end
