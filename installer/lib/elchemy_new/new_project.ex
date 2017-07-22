defmodule Elchemy.NewProject do
  use Elchemy.Generator

  template :new, [
    {:eex, "new/mix.exs", "mix.exs"},
    {:eex, "new/README.md", "README.md"},
    {:eex, "new/config/config.exs", "config/config.exs"},
    {:eex, "new/elm/AppName.elm", "elm/:app_module.elm"}
  ]

end
