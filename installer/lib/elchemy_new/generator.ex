defmodule Elchemy.Generator do
  @callback generate(Project.t) :: any

  alias Elchemy.Project
  import Mix.Generator

  defmacro __using__(_env) do
    quote do
      @behaviour unquote(__MODULE__)
      import unquote(__MODULE__)
      import Mix.Generator
      Module.register_attribute(__MODULE__, :templates, accumulate: true)
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(env) do
    root = Path.expand("../../templates", __DIR__)
    templates_ast = for {name, mappings} <- Module.get_attribute(env.module, :templates) do
      for {format, source, _} <- mappings, format != :keep do
        path = Path.join(root, source)
        quote do
          @external_resource unquote(path)
          def render(unquote(name), unquote(source)), do: unquote(File.read!(path))
        end
      end
    end

    Macro.to_string(templates_ast)
    |> IO.inspect

    quote do
      unquote(templates_ast)
      def template_files(name), do: Keyword.fetch!(@templates, name)
    end
  end


  defmacro template(name, mappings) do
    quote do
      @templates {unquote(name), unquote(mappings)}
    end
  end


  def copy_from(%Project{} = project, mod, name) when is_atom(name) do
    mapping = mod.template_files(name)

    IO.inspect mapping
    project_location ="project"
    for {format, source, target_path} <- mapping do
      target = Project.join_path(project, project_location, target_path)
      IO.inspect target

      case format do
        :eex  ->
          contents = EEx.eval_string(mod.render(name, source), project.binding, file: source)
          create_file(target, contents)
      end
    end
  end
end
