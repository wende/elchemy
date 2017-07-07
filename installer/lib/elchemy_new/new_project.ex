defmodule Elmchemy.NewProject do
  use Elmchemy.Generator

  template :new, [
    {:eex, "new/mix.exs", "mix.exs"},
    {:eex, "new/README.md", "README.md"},
    {:eex, "new/config/config.exs", "config/config.exs"},
    {:eex, "new/lib/elm/AppName.elm", "lib/elm/:app_module.elm"}
  ]

end
