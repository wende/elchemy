defmodule Mix.Tasks.Elmchemy do
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
     |> iterate_specs()}
  end

  defp iterate_specs(specs) when is_list(specs),
    do: Enum.map(specs, &gen_elm/1) |> Enum.filter(fn a -> a != "\n" end)
  defp iterate_specs(nil), do: []

  defp gen_elm({{_name, _arity}, [{:type, _ , :bounded_fun, _spec}] }) do
    "\n"
  end
  defp gen_elm({{name, arity}, specs}) do
    "#{to_string name}_#{arity} : " <>
    Enum.join(Enum.map(specs, &gen_spec/1))
  end
  defp gen_elm({{:erts_internal, name, arity}, specs}) do
    gen_elm({{name, arity}, specs})
  end
  defp gen_elm({{:erlang, name, arity}, specs}) do
    gen_elm({{name, arity}, specs})
  end


  defp gen_spec({:type, _line, type, args}) do
    case {type, args} do
      {:fun, [args, result]} ->
        IO.inspect args
        "#{gen_spec args} -> #{gen_spec result}"
      {:product, args} ->
        args
        |> Enum.map(&gen_spec/1)
        |> Enum.join(" -> ")
      {:union, args} ->
        args
        |> Enum.map(&gen_spec/1)
        |> Enum.join(" | ")
      {any, args} when is_list args ->
        "#{elmify_type any, Enum.map(args, &gen_spec/1)}"
      {any, arg} ->
        elmify_type(any, [arg])
    end
  end
  defp gen_spec({:user_type, _line, name, args}),
    do: (name |> to_string |> String.capitalize) <>
      (Enum.map(args, &gen_spec/1) |> Enum.join("."))
  defp gen_spec({:ann_type, _line, [_typename, type]}), do: gen_spec(type)
  defp gen_spec({:remote_type, _line, path}),
    do: path |> join_path
  defp gen_spec([]), do: "[]"

  defp gen_spec({type, _line, value}) do
    elmify_type(type, [value])
  end
  # NIF. Nothing to do here
  defp gen_spec(other) when is_list(other),
    do: "Nif"

  defp join_path(path) do
    path
    |> Enum.reverse
    |> List.delete_at(0)
    |> Enum.reverse
    |> Enum.map(&extract_path/1)
    |> Enum.join(".")
  end
  defp extract_path({:atom, _, name}), do: name


  defp elmify_type(type, rest) do
    case {type, rest} do
      {:map, []} -> "Map #{rest}"
      {:map, [h | t] = types} ->
        if is_binary(h) && h |> String.contains?("=") do
          "{" <> Enum.join(types, ",") <> ""
        else
          "Map #{h}"
        end

      {:tuple, _} -> "(#{Enum.join(rest, ", ")})"
      {:pos_integer, []} -> "Integer"
      {:atom, []} -> "Atom"
      {:atom, [val]} -> "Atom #{val}"
      {:boolean, []} -> "Bool"
      {:module, []} -> "Module"
      {:term, []} -> "a"
      {:list, _} -> "List " <> Enum.join(rest, " ")
      {:any, []} -> "Any"
      {:timeout, []} -> "Timeout"
      {:no_return, []} -> "Nothing"
      {:byte, []} -> "Byte"
      {:integer, []} -> "Int"
      {:integer, [_value]} -> "Int"
      {:arity, []} -> "Int"
      {:string, []} -> "String"
      {:node, []} -> "Atom"
      {:nil, []} -> "Nothing"
      {:pid, []} -> "Pid"
      {:nonempty_string, []} -> "String"
      {:non_neg_integer, []} -> "Int"
      {:binary, []} -> "Binary"
      {:binary, any} -> "Binary"
      {:var, [:_]} -> "Any"
      {:var, [name]} -> "#{name}"
      {:nonempty_list, [type]} -> "List " <> type
      {:record, [type]} -> "Record " <> to_string type
      {:bounded_fun, _} -> "boun"
      {:function, []} -> "(Any -> Any)"
      {:fun, []} -> "(Any -> Any)"
      {:char, []} -> "Char"
      {:map_field_exact, [key, value]} -> "#{key} = #{value}"
      {:iolist, []} -> "IOList"
      {:iodata, []} -> "IOData"
      {:map_field_assoc, [key, val]} -> "MapFieldAssoc #{key} #{val}"
      {:neg_integer, _} -> "Int"
      {:float, []} -> "Float"
      {:reference, []} -> "Reference"
      {:bitstring, []} -> "String"
      {:port, []} -> "Port"
      {:number, []} -> "Float"
      {:maybe_improper_list, []} -> "List"
      {:range, [from, to]} -> "Range #{from} #{to}"
      {:type, [:any]} -> "Any"
      {:identifier, []} -> "Int"
      # {:nonempty_list}
    end
  end
  defp elmify_name(name) do
    Macro.camelize name
  end


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
