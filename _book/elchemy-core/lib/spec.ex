defmodule Elchemy.Spec do
  require Logger
  def find(module, function, arity) do
    case Code.ensure_loaded(module) do
      {:error, _} -> raise "Module #{module} couldn't be found"
      {:module, _} -> :ok
    end

    possible_result =
      module
      |> Kernel.Typespec.beam_specs()
      |> (&(&1 || raise "Module #{module} wasn't compiled before and cannot be used")).()
      |> Enum.filter(fn {{name, _ar}, _} -> name == function end)
    result =
      possible_result
      |> Enum.find(fn {fun, _} -> fun == {function, arity} end)
    if result == nil do
      raise """
      No function #{inspect module}.#{function}/#{arity} found
      Maybe you meant one of these:
      #{specs_to_readable(possible_result)}
      """
    else
      result
    end
  end

  def compare!(left = {{_, _}, [{_, line, _, _}]}, {m, f, a}, mod1, mod2 \\ nil) do
    right = find(m, f, a)
    mod2 = mod2 || mod1
    result = compare(left, right, mod1, mod2)
    if result == :ok do
      :ok
    else
      raise Elchemy.SpecError, message: """
      Type definition mismatch at #{inspect mod1} line #{line}
      These type signatures are different:
        #{gen_elm left, mod1}
        #{gen_elm right, mod2}
      Because of #{inspect result}
      """
    end
  end
  @doc """
  Check if left spec is a subtype of a right one
  """
  def compare(l, r, mod1, mod2 \\ nil)
  def compare(l = {{_, arity1}, [spec1]}, {{f, arity2}, [spec2 | rest]}, mod1, mod2) do
    mod2 = mod2 || mod1
    compared =
      spec1
      |> lambdify_functions()
      |> do_compare(spec2, mod1, mod2)

    cond do
      arity1 != arity2 ->
        {:error, "functions have different arity"}
      compared == :ok ->
         :ok
      rest == [] ->
        compared
      true ->
        compare(l, {{f, arity2}, rest}, mod1, mod2)
    end
  end
  defp do_compare(same, same, _mod1, _mod2), do: :ok
  defp do_compare(left, {:ann_type, _, [_, type]}, m1, m2),
    do: do_compare(left, type, m1, m2)
  defp do_compare(left, {:type, {:as_boolean, right, _}}, m1, m2),
    do: do_compare(left, right, m1, m2)
  defp do_compare(l, {:type, _, :bounded_fun, [fun | _constraints]}, m1, m2) do
    do_compare(l, fun, m1, m2)
  end
  defp do_compare({:type, _, :list, [{:type, _, :any, []}]}, {:type, _, :list, []}, _m1, _m2) do
    :ok
  end
  defp do_compare(t1 = {:type, _, :map, args1}, t2 = {:type, _, :map, args2}, m1, m2) do
    if length(args1) != length(args2) do
      {:error, "#{gen_spec t1, m1} has different amount of fields than #{gen_spec t2, m2}"}
    else
      [Enum.sort(args1), Enum.sort(args2)]
      |> Enum.zip()
      |> Enum.map(fn {arg1, arg2} -> do_compare(arg1, arg2, m1, m2) end)
      |> Enum.find(fn a -> a != :ok end)
      |> (&(&1 || :ok)).()
    end
  end
  defp do_compare({:type, _, type, []}, {:type, _, type, []}, _, _), do: :ok
  defp do_compare(_, {:type, _, :any, []}, _, _), do: :ok
  defp do_compare(_, {:type, _, :term, []}, _, _), do: :ok
  defp do_compare(_, {:var, _, _}, _, _), do: :ok
  defp do_compare(t1 = {:type, _, type, args1}, t2 = {:type, _, type, args2}, m1, m2) do
    if length(args1) != length(args2) do
      {:error, "#{gen_spec t1, m1} is not the same length as #{gen_spec t2, m2}"}
    else
      [args1, args2]
      |> Enum.zip()
      |> Enum.map(fn {arg1, arg2} -> do_compare(arg1, arg2, m1, m2) end)
      |> Enum.find(fn a -> a != :ok end)
      |> (&(&1 || :ok)).()
    end
  end
  defp do_compare(a = {:type, _, :union, args}, b, m1, m2) do
    if Enum.any?(args, fn a -> do_compare(a, b, m1, m2) == :ok end)  do
      :ok
    else
      {:error, "none of #{gen_spec a, m1} is a subtype of #{gen_spec b, m2}"}
    end
  end
  defp do_compare(a, b = {:type, _, :union, args}, m1, m2) do
    if Enum.any?(args, fn b -> do_compare(a, b, m1, m2) == :ok end)  do
      :ok
    else
      {:error, "none of #{gen_spec a, m1} is a subtype of #{gen_spec b, m2}"}
    end
  end
  defp do_compare(t1 = {:type, _, _type1, _arg1}, t2 = {:type, _, _type2, _arg2}, m1, m2)
  do
    compare_types(t1, t2, m1, m2)
  end
  defp do_compare({:var, _, _}, {:var, _, _}, _, _), do: :ok
  defp do_compare({:user_type, _, name, args1}, {:user_type, _, name, args2}, m1, m2) do
    [args1, args2]
    |> Enum.zip()
    |> Enum.map(fn {arg1, arg2} -> do_compare(arg1, arg2, m1, m2) end)
    |> Enum.find(fn a -> a != :ok end)
    |> (&(&1 || :ok)).()
  end
  defp do_compare({:user_type, _, name, _}, b, m1, m2) do
    do_compare(resolve_type(name, m1), b, m1, m2)
  end
  defp do_compare(a, {:user_type, _, name, _}, m1, m2) do
    do_compare(a, resolve_type(name, m2), m1, m2)
  end
  defp do_compare({:remote_type, _, path}, {:remote_type, _, path}, _, _) do
    :ok
  end
  defp do_compare({:remote_type, _, path}, b, _m1, m2) do
    {module, name} = get_path_and_name(path)
    # We replace context module because multiple remote_types
    do_compare(resolve_type(name, module), b, module, m2)
  end
  defp do_compare(a, {:remote_type, _, path}, m1, _m2) do
    {module, name} = get_path_and_name(path)
    # We replace context module because multiple remote_types
    do_compare(a, resolve_type(name, module), m1, module)
  end
  defp do_compare(t1 = {_type2, _, _val}, t2 = {:type, _, _, _}, m1, m2),
    do: do_compare(t2, t1, m2, m1)
  defp do_compare(t1 = {:type, _, type1, _},  t2 = {type2, _, _val}, m1, _m2) do
    cond do
      type1 == type2 -> :ok

      type2 == :integer && type1 == :number -> :ok
      type2 == :integer && type1 == :pos_integer -> :ok
      type2 == :integer && type1 == :non_neg_integer -> :ok
      type2 == :integer && type1 == :neg_integer -> :ok
      type2 == :integer && type1 == :float -> :ok

      type2 == :float && type1 == :number -> :ok
      type2 == :float && type1 == :integer -> :ok

      true -> {:error, "Type instance #{inspect t2} is not of type #{gen_spec t1, m1} (#{inspect t1})"}
    end
  end

  defp compare_types(t1 = {:type, _, type1, arg1}, t2 = {:type, _, type2, arg2}, m1, m2) do
    case {{type1, arg1}, {type2, arg2}} do
      {same, same} -> :ok

      {{:list, [{:type, _, :any, _}]}, {:list, []}} -> :ok
      {{:list, []}, {:list, [{:type, _, :any, _}]}} -> :ok

      {{:string, []}, {:list, []}} -> :ok
      {{:string, []}, {:list, [b]}} ->
        do_compare({:type, 0, :integer, []}, b, m1, m2)
      {{:list, []}, {:string, []}} -> :ok
      {{:list, [a]}, {:string, []}} ->
        do_compare(a, {:type, 0, :integer, []}, m1, m2)

      {{:integer, []}, {:float, []}} -> :ok
      {{:integer, []}, {:neg_integer, []}} -> :ok
      {{:integer, []}, {:non_neg_integer, []}} -> :ok
      {{:integer, []}, {:pos_integer, []}} -> :ok
      {{:integer, []}, {:number, []}} -> :ok
      _ ->
        {:error, "type #{gen_spec t1, m1} is not a subtype of #{gen_spec t2, m2}"}
    end
  end
  def gen_elixir({{name, _}, [spec]}, _mod) do
    Kernel.Typespec.spec_to_ast(name, spec)
    |> Macro.to_string()
  end

  def gen_elm({{name, _arity}, [{:type, _ , :bounded_fun, [spec | _constraint]}]}, mod) do
    "#{to_string name} : " <> gen_spec(spec, mod)
  end
  def gen_elm({{name, _arity}, [{:type, _ , :fun, [args , result]}]}, mod) do
    "#{to_string name} : " <>
    gen_spec(args, mod) <> " -> " <>
    gen_spec(result, mod)
  end
  def gen_elm({{name, _arity}, specs}, mod) do
    "#{to_string name} : " <>
    Enum.join(Enum.map(specs, &gen_spec(&1, mod)), " | ")
  end
  def gen_elm({{:erts_internal, name, arity}, specs}, mod) do
    gen_elm({{name, arity}, specs}, mod)
  end
  def gen_elm({{:erlang, name, arity}, specs}, mod) do
    gen_elm({{name, arity}, specs}, mod)
  end

  defp gen_spec({:type, _line, type, args}, mod) do
    case {type, args} do
      {:fun, [args, result]} ->
        "(#{gen_spec args, mod} -> #{gen_spec result, mod})"
      {:product, args} ->
        (args
        |> Enum.map(&gen_spec(&1, mod))
        |> Enum.join(" -> "))
      {:union, args} ->
        args
        |> Enum.map(&gen_spec(&1, mod))
        |> Enum.join(" | ")
      {any, args} when is_list args ->
        "#{elmify_type any, Enum.map(args, &gen_spec(&1, mod))}"
      {any, arg} ->
        elmify_type(any, [arg])
    end
  end
  defp gen_spec({:user_type, _line, name, _args}, mod),
    do: resolve_type(name, mod) |> gen_spec(mod)
  defp gen_spec({:ann_type, _line, [_typename, type]}, mod),
   do: gen_spec(type, mod)
  defp gen_spec({:remote_type, _line, path}, _mod) do
    {module, name} = get_path_and_name(path)
    resolve_type(name, module) |> gen_spec(module)
  end
  defp gen_spec([], _mod), do: "[]"

  defp gen_spec({type, _line, value}, _mod) do
    elmify_type(type, [value])
  end
  defp gen_spec([{:type, _, :constraint, _} | _], _) do
    ""
  end

  defp get_path_and_name(ast) when is_list(ast) do
    rev = Enum.reverse(ast) |> List.flatten
    [{:atom, _, name} | revpath] = rev
    path =
      revpath
      |> Enum.map(fn {:atom, _, module} -> module end)

    path =
      if (length(path) == 1) && !(path |> hd |> to_string |> String.starts_with?("Elixir.")) do
        path |> hd
      else
        Module.concat(path)
      end
    {path, name}
  end

  defp elmify_type(type, rest) do
    case {type, rest} do
      {:map, []} -> "Dict k v"
      {:map, [h | _t] = types} ->
        if is_binary(h) && h |> String.contains?("=") do
          "{" <> Enum.join(types, ",") <> "}"
        else
          "Map #{h}"
        end

      {:tuple, _} -> "(#{Enum.join(rest, ", ")})"

      {:atom, []} -> "Atom"
      {:atom, [val]} -> ":#{val}"

      {:boolean, []} -> "Bool"

      {:module, []} -> "Module"

      {:term, []} -> "term"
      {:any, []} -> "any"
      {:var, [:_]} -> "a"
      {:var, [name]} -> "#{String.downcase (to_string name)}"
      {:type, [:any]} -> "any"

      {:list, []} -> "List any"
      {:list, _} -> "List " <> Enum.join(rest, " ")
      {:nonempty_maybe_improper_list, [content, _termination]} -> "NonemptyList #{content}"
      {:nonempty_list, [type]} -> "NonEmptyList " <> type
      {:maybe_improper_list, []} -> "List"

      {:timeout, []} -> "Timeout"

      {:no_return, []} -> "()"

      {:byte, []} -> "Byte"

      {:integer, []} -> "Int"
      {:integer, [value]} -> "Int #{value}"
      {:neg_integer, _} -> "NegativeInt"
      {:non_neg_integer, []} -> "NonNegativeInt"
      {:pos_integer, []} -> "PositiveInt"
      {:arity, []} -> "Arity"

      {:number, []} -> "number"
      {:float, []} -> "Float"

      {:string, []} -> "List Int"
      {:nonempty_string, []} -> "NonEmptyString"
      {:binary, []} -> "Binary"
      {:binary, _any} -> "Binary"

      {:node, []} -> "Node"
      {:nil, []} -> "Nothing"
      {:pid, []} -> "Pid"

      {:record, [type]} -> "Record " <> to_string type

      {:function, []} -> "(Any -> Any)"
      {:fun, []} -> "(Any -> Any)"
      {:bounded_fun, rest} -> Enum.join rest

      {:char, []} -> "IntChar"

      {:map_field_exact, [key, value]} -> "#{key} => #{value}"
      {:map_field_assoc, [key, val]} -> "MapFieldAssoc #{key} #{val}"

      {:iolist, []} -> "IOList"
      {:iodata, []} -> "IOData"
      {:reference, []} -> "Reference"
      {:bitstring, []} -> "BitString"
      {:port, []} -> "Port"
      {:range, [from, to]} -> "Range #{from} #{to}"
      {:identifier, []} -> "Int"
      # {:nonempty_list}
    end
  end

  defp resolve_type(type, module) when is_atom(type) do
    beam_types =
      module
      |> Kernel.Typespec.beam_types()

    if beam_types == nil do
      throw "There is no #{type} in #{inspect module}"
    end
    resolved =
      beam_types
      |> Enum.find(fn
        {:type, {name, _definition, _}} -> name == type
        {:type, {name, _definition, _}, _args} -> name == type
        {:opaque, _} -> false
        {:typep, _} -> false
      end)
    case resolved do
      {:type, {^type, {:type, _, :any, []}, []}} ->
        {:var, 0, type}
      # We don't care for arguments because you can't have two same type names in Elm
      {:type, {^type, definition, _args}} ->
        definition
      nil ->
        Logger.warn "No type with name #{type} in module #{inspect module}. Resolving to :any"
        {:type, 0, :any, []}
    end
  end

  defp lambdify_functions(
    {:type, l1, :fun, [
      {:type, l2, :product, [a]},
      {:type, _l3, :fun, [
          {:type, _l4, :product, [b]},
          return
      ]}
    ]}
  ) do
    case lambdify_functions(return) do
      {:type, _, :fun, [
          {:type, _, :product, args},
          return
        ]} ->
        {:type, l1, :fun, [
            {:type, l2, :product, [a,b] ++ args},
            lambdify_functions(return)
          ]}
       _other ->
        {:type, l1, :fun, [
            {:type, l2, :product, [a, b]},
            lambdify_functions(return)
          ]}
    end
  end
  defp lambdify_functions({:type, l1, type, args}) do
    {:type, l1, type, Enum.map(args, &lambdify_functions/1)}
  end
  defp lambdify_functions(l), do: l

  defp specs_to_readable(specs) do
    for {{name, arity}, defs} <- specs do
      "#{name}/#{arity}:\n" <>
      (for d <- defs do
        Kernel.Typespec.spec_to_ast(name, d) |> Macro.to_string()
      end |> Enum.join("\n"))
    end
    |> Enum.join("\n")
  end
end
defmodule Elchemy.SpecError do
  defexception [:message]
end
