defmodule ElchemyInit do

  @deps_directory_depth 3

  def init(project, {__MODULE, _}) do
    project
    |> put_in([:compilers], [:elchemy | (project[:compilers] || Mix.compilers())])
    |> put_in([:elchemy_path], "elm")
    |> put_in([:deps], project[:deps] ++ elm_deps())
  end

  def elm_deps() do
    if File.exists?("elm-deps") do
        find!("elm-deps", @deps_directory_depth)
        |> check_mix_file()
        |> Enum.map(fn {app_name, path} ->
            {app_name, path: path, override: true}
        end)
    else
        []
    end
  end

  def check_mix_file(paths) do
    Enum.map paths, fn path ->
      mix_file = Path.join(path, "mix.exs")
      if File.exists?(mix_file) do
        {parse_app_name(mix_file), path}
      else
        {app_name, version} = app_info_from_path(path)
        create_mix_file(mix_file, app_name, version)
        {app_name, path}
      end
    end
  end

  def create_mix_file(mix_file, app_name, version) do
    module_name = app_name
    |> Atom.to_string
    |> String.split("_")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join("")

    content = """
defmodule #{module_name}.Mixfile do
  use Mix.Project

  def project do
    [app: #{inspect app_name},
     version: "#{version}",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     elixirc_paths: ["src"],
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps, do: [{:elchemy, override: false}]
end
    """
    IO.puts "Creating mix file #{mix_file}"

    File.write!(mix_file, content)
  end

  def app_info_from_path(path) do
    [_, _, repo_name, version] = path |> Path.split

    app_name = repo_name
    |> String.replace(~r"[-]", "_")
    |> String.replace(~r"[^a-zA-Z0-9_]", "")
    |> String.to_atom

    {app_name, version}
  end

  def find!(dir, depth), do: find!([], dir, depth)
  def find!(dirs, dir, 0), do: [dir | dirs]
  def find!(dirs, dir, depth) do
    dir
    |> File.ls!
    |> Enum.map(&Path.join(dir, &1))
    |> Enum.filter(&File.dir?/1)
    |> Enum.reduce(dirs, &find!(&2, &1, depth - 1))
  end

  def parse_app_name(mix_file) do
    contents = File.read!(mix_file)
    {app_name, _} = ~r"app:(.*?)," |> Regex.run(contents, capture: :all_but_first)
    |> List.first |> String.trim |> Code.eval_string
    app_name
  end
end

ElchemyInit