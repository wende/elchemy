defmodule Elmchemy.Glue do

  defmacro defcurryp(definition, opts \\ [], do: body) do
    {fun, args} = Macro.decompose_call(definition)
    quote do
      defp unquote(fun)() do
        unquote(curry(fun, args))
      end
      defp unquote(fun)(unquote_splicing(args)) do
        unquote(body)
      end
    end

  end

  defmacro defcurry(definition, opts \\ [], do: body) do
    {fun, args} = Macro.decompose_call(definition)
    quote do
      def unquote(fun)() do
        unquote(curry(fun, args))
      end
      def unquote(fun)(unquote_splicing(args)) do
        unquote(body)
      end
    end
  end

  defp curry(fun, args), do: curry(fun, args, args)
  defp curry(fun, [h | args], all) do
    quote do fn unquote(h) -> unquote(curry(fun, args, all)) end end
  end
  defp curry(fun, [], args) do
    quote do: unquote(fun)(unquote_splicing(args))
  end

end
