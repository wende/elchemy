defmodule Elmchemy.Mixfile do
  use Mix.Project

  def project do
    [app: :elmchemy,
     name: "Elmchemy Compiler",
     description: "Mix compiler wrapper around Elmchemy project",
     version: "0.1.2",
     elixir: "~> 1.4",
     description: "",
     package: package(),
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end
  defp package do
    # These are the default files included in the package
    [
      name: :elmchemy,
      files: ["lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*", "license*"],
      maintainers: ["Krzysztof Wende", "Tomasz Cichocinski"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/wende/elmchemy"}
    ]
  end
  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    []
  end
end
