defmodule ElmchemyHack do

  defp get_definitions(content) do
    ~r/def (\w+)/
    |> Regex.scan(content)
  end

  defmacro __before_compile__(env) do
    IO.inspect env
    IO.inspect __CALLER__.module

    File.read!(env.file)
    |> IO.inspect
    |> get_definitions
    |> IO.inspect
  end

end

defmodule Elmchemy do

  defmacro __using__(_) do
    quote do
      require Elmchemy
      import Elmchemy
      require Elmchemy.Glue

      import Kernel, except: [{:"++", 2}]

      import Elmchemy.Glue
      import Kernel, except: [
        {:'++', 2}
      ]
      alias Elmchemy.{
        XBasics,
        XString,
        XMaybe,
        XChar,
        XTuple,
        XResult
      }
      import_std(Elmchemy.XBasics)
    end
  end

  defmacro import_std(mod, _opts \\ []) do
    if __CALLER__.module != Macro.expand(mod, __ENV__) do
      quote do
        import unquote(mod)
      end
    else
      quote do
        :ok
      end
    end
  end
end
