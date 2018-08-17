defmodule Elchemy.Mixfile do
  use Mix.Project

  def project do
    [app: :elchemy,
     name: "Elchemy Compiler",
     description: "Mix compiler wrapper around Elchemy project",
     version: "0.7.4",
     elixir: "~> 1.4",
     description: "",
     package: package(),
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     elixirc_paths: ["elm", "lib", "elm-deps"],
     elchemy_path: "elm",
     deps: deps()]
  end
  defp package do
    # These are the default files included in the package
    [
      name: :elchemy,
      files: ["lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*", "license*"],
      maintainers: ["Krzysztof Wende"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/wende/elchemy"}
    ]
  end
  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: [:logger]]
  end

  defp deps do
    []
  end
end
