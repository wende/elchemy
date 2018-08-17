# Installation

You can install Elchemy using [npm](https://www.npmjs.com/) with
```
npm install -g elchemy
```

To integrate Elchemy with your project you need to execute:

```
elchemy init
```

Inside your elixir project directory.  
If you don't have a project created, you need to first create it. It's advised to use  [Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#our-first-project) for that.

Assuming the simplest example project called `my_project` the standard path would be:

```
mix new my_project
cd my_project
elchemy init
```

Then open your `mix.exs` file inside project root directory. And add:
```elixir
|> Code.eval_file("elchemy.exs").init
```
At the end of your `project/0` function definition. Like so:  
(As of OTP 21.0 and above you must also add `@compile :tuple_calls` at the top of the Mix module. It is caused by tuple calls support being removed from newer versions of Erlang)
Before:
```elixir
defmodule MyProject.Mixfile do
  use Mix.Project

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
  ...
```
After:
```elixir
defmodule MyProject.Mixfile do
  use Mix.Project
  @compile :tuple_calls

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ] |> Code.eval_file("elchemy.exs").init
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
  ...
```
