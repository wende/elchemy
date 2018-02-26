defmodule Elchemy.CLI do
  require Mix.Generator

  @help """
  Available commands
      new <PROJECT_NAME>
          Start a new project

      init
          Add Elchemy to an existing project

      compile [INPUT_DIR] [OUTPUT_DIR] [--unsafe]
          Compile Elchemy source code

      clean
          Remove temporary files

  Options
      --help, -h Include a rainbow
      --version, -v Print Elchemy's version
      --verbose

  """

  def main(args \\ []) do
    {opts, arg, _} = parse_options(args)

    IO.puts("Using elchemy v#{version()}\n")

    handle(arg)

    if opts[:help] do
      help()
    end

    if opts[:version] do
      version()
    end
  end

  def handle([]), do: help()

  def handle(["new", dir | _]) do
    System.cmd("mix", ["new", "dir", "--force"])
    File.cd(dir)
    handle(["init"])
  end

  def handle(["init"]), do: handle(["init", File.cwd!])

  def handle(["init", source_dir | _]) do
    if File.exists?("./mix.exs") do
      vsn = version()

      IO.puts("Getting latest version of elchemy")

      System.cmd("mix", [
        "archive.install",
        "https://github.com/wende/elchemy/releases/download/#{vsn}/elchemy-#{vsn}.ez",
        "--force"
      ])

      Mix.Generator.create_directory("elm")
      Mix.Generator.create_directory("test")

      IO.puts("Adding basic project files")
      [
        {"#{source_dir}/templates/elm-package.json", "./elm-package.json"},
        {"#{source_dir}/templates/elchemy.exs", "./elchemy.exs"},
        {"#{source_dir}/templates/Hello.elm", "./elm/Hello.elm"},
        {"#{source_dir}/templates/elchemy_test.exs", "./test/elchemy_test.exs"}
      ]
      |> Enum.each(&copy_file/1)

      IO.puts("Adding entires to .gitignore")
      File.write(".gitignore", "\nelm-deps\nelm-stuff", [:append])

      IO.puts("""

      Elchemy #{vsn} initialized. Make sure to add:

          |> Code.eval_file(\"elchemy.exs\").init

      to your mix.exs file as the last line of the project() function.
      This pipes the project keyword list to the elchemy init function to configure some additional values.
      Then run `mix test` to check if everything went fine.

      """)
    else
      IO.write(:stderr, "ERROR: No elixir project found. Make sure to run init in a project")
    end
  end

  def handle(["clean" | _]) do
    IO.puts("clean")
  end

  def time_to_unix()

  def handle(["compile", dir | _]) do
    # Create ./.elchemy if doesn't exist
    Mix.Generator.create_directory ".elchemy"

    default_time = "def 1995-04-10T23:25:02+01:00"
    mtime = case File.read(".elchemy/mtime") do
      {:err, _} -> {:ok, default_time}
      a -> a
    end 
    |> map_snd(&DateTime.from_iso8601/1)
    |> flatten
    |> DateTime.to_unix

    File.write!(".elchemy/output", "")
    # if [ ! -d ./elm-deps ] || [[ !  $(find ./elm-package.json -newermt "$MTIME") == "" ]]; then
    #     if ! hash elm-github-install 2>/dev/null; then
    #         IO.puts "No elm-github-install found. Installing..."
    #         npm i -g elm-github-install@1.6.1
    #     fi
    #     echo "-- Downloading Elchemy deps --"
    #     elm-install
    # fi
    # # Copy all elixir files that are inside packages
    IO.puts "-- Copying Elixir native files --" 
    # for f in `{ find -L elm-stuff/packages -name "*.ex*" | grep -v "\.elchemy\.ex" ;}`
    # do
    #   if [ $VERBOSE = true ]; then
    #     echo "FOUND $f"
    #   fi
    #   file="${file/^elm\//lib\//}"
    #   file=$(create_file $f)
    #   if [ $VERBOSE = true ]; then
    #      echo "TO $file"
    #    fi
    #   cp $f $file
    # done
    # i=0
    IO.puts "-- Compiling Elm files --"
    # Find all elm files inside packages and compile them

    source = Path.wildcard("#{dir}/**/*.elm") 
    |> Enum.filter(fn path -> 
      String.contains?("elm-stuff") or String.contains?("#.")
    end)

    deps = Path.wildcard("elm-stuff/packages/**/*.elm") 
    |> Enum.filter(fn path -> 
      String.contains?("/tests/") or String.contains?("/example/") 
    end)

    source ++ deps 
    |> Enum.filer(fn path -> get_mtime(path) > mtime end)
    |> IO.inspect
    # find $2 -name "*.elm" -newermt "$MTIME" | grep -v "elm-stuff" | grep -v "#." 
    # find -L elm-stuff/packages -name "*.elm" -newermt "$MTIME" | grep -v "/tests/" | grep -v "/example/"
    # do
    #     if [[ ${f} == *"elm-lang"* ]] || [[ ${f} == *"Elchemy.elm"* ]]; then
    #         continue
    #     fi
          # We don't need to typecheck deps again
    #     if [[ ${f} != *"elm-stuff"* ]] && ! [[ $* == *--unsafe* ]]; then
    #         (echo n | elm-make $f --output .elchemy/output_tmp.js) || { echo 'Type Check failed' ; exit 1; }
    #         rm .elchemy/output_tmp.js
    #     fi
    #     i=$((i+1))
    #     echo "#$i"
    #     cat $f >> .elchemy/output
    # done
    IO.puts "-- Linking files --"
    # node --max_old_space_size=8192 $SOURCE_DIR/elchemy.js .elchemy/output .elchemy/elixir_output .elchemy/cache.json
    # current_file=""
    # while IFS= read -r line; do
    #     if [[ $line =~ ">>>>" ]]; then
    #         current_file="${line/\/\///}"
    #         current_file="${current_file/>>>>/}"
    #         echo "Linking: $current_file"
    #         current_file="${current_file/$2\//$3/}"
    #         current_file="${current_file%%.elm}.elchemy.ex"
    #         current_file=$(echo ${current_file} | perl -pe 's/([a-z0-9])([A-Z])/$1_\L$2/g')
    #         current_file=$(create_file $current_file)
    #         echo "To: $current_file"
    #         else
    #         if [ "$current_file" != "" ]; then
    #             printf '%s\n' "$line" >> "$current_file"
    #         fi
    #     fi
    # done < .elchemy/elixir_output

    DateTime.utc_now()
     |> DateTime.to_iso8601
     |> (&File.write(".elchemy/mtime", &1)).()

  end

  def flatten({:ok, {:ok, value, _}}), do: value
  def flatten(a), do: a

  def map_snd({:ok, b}, fun), do: {:ok, fun.(b)}
  def map_snd(a, _), do: a

  def get_or_else({:ok, value}, _), do: value
  def get_or_else(_, default), do: default

  def help(), do: IO.puts(@help)

  defp get_mtime(path) do  
    case File.stat(path, [time: :posix]) do 
      {:ok, info} -> info.mtime
      _ -> DateTime.utc_now |> DateTime.to_unix
    end
  end 

  defp copy_file({src, dest}) do
    File.cp!(src, dest, fn _, _ -> false end)
  end

  defp version() do
    {:ok, vsn} = :application.get_key(:elchemy, :vsn)
    List.to_string(vsn)
  end

  defp parse_options(args) do
    OptionParser.parse(
      args,
      switches: [help: :boolean, version: :boolean, verbose: :boolean],
      aliases: [h: :help, v: :version]
    )
  end
end
