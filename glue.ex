defmodule Glue do

  defmacro defun(definition, opts \\ [], do: body) do
    {fun, args} = Macro.decompose_call(definition)
    quote do
      def unquote(fun)() do
        unquote(curry(args, body))
      end
    end
  end

  defp curry([h | args], body) do
    quote do fn unquote(h) -> unquote(curry(args, body)) end end
  end
  defp curry([], body), do: body


end
