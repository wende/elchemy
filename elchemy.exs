defmodule ElchemyInit do
  def init(project, {__MODULE, _}) do
    project
    |> put_in([:compilers], [:elchemy | (project[:compilers] || [])])
    |> put_in([:elchemy_path], "elm")
    |> put_in([:deps], project[:deps] ++ deps())
  end

  def deps() do
    if File.exists?("elm-deps") do
        find!("elm-deps")
        |> Enum.map(fn path ->
            {app_name(path), path: path}
        end)
    else
        []
    end
  end

  def find!(dir), do: find!([], dir)
  def find!(dirs, dir) do
    files = dir |> File.ls!
    if Enum.member?(files, "mix.exs") do
      [dir | dirs]
    else
      files
      |> Enum.map(&Path.join(dir, &1))
      |> Enum.filter(&File.dir?/1)
      |> Enum.reduce(dirs, &find!(&2, &1))
    end
  end

  def app_name(path) do
    mix_file = Path.join(path, "mix.exs") |> File.read!
    {app_name, _} = ~r"app:(.*?)," |> Regex.run(mix_file, capture: :all_but_first)
    |> List.first |> String.trim |> Code.eval_string
    app_name
  end
end

ElchemyInit