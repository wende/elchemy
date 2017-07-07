defmodule Elmchemy.Project do
  defstruct app_name: "", app_module: nil

  alias __MODULE__

  def new(app_name) do
    %__MODULE__{app_name: app_name, app_module: Macro.camelize(app_name)}
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
