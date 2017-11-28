defmodule Mix.Tasks.Elchemy do
  use Mix.Task

  def run(["init"]) do
    load_all(:code.get_path)
    IO.inspect :code.all_loaded
  end
  def run(["refresh"]) do
    load_all(['.'])
    :code.all_loaded
    |> Enum.map(&elmize/1)
    |> Enum.map(&IO.inspect/1)
  end

  defp elmize({module, _beamfile}) do
    {module, IO.inspect(module)
     |> Kernel.Typespec.beam_specs()
     |> iterate_specs(module)}
  end

  defp iterate_specs(specs, mod) when is_list(specs),
    do: Enum.map(specs, &(Elchemy.Spec.gen_elm(&1, mod))) |> Enum.filter(fn a -> a != "\n" end)
  defp iterate_specs(nil, _mod), do: []

    defp load_all(paths) do
      for p <- paths, f <- :filelib.wildcard(p ++ '/*.beam') do
        f
        |> :filename.basename()
        |> :filename.rootname()
        |> :erlang.list_to_atom()
        |> :code.ensure_loaded()
      end
    end
end
