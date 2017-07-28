defmodule Elchemy.NewProject do
  use Elchemy.Generator

  template :new, [
    {:eex, "new/mix.exs", "mix.exs"},
    {:eex, "new/README.md", "README.md"},
    {:eex, "new/config/config.exs", "config/config.exs"},
    {:eex, "new/elm/AppName.elm", "elm/:app_module.elm"},
    {:eex, "new/elm-package.json", "elm-package.json"},
    {:eex, "new/test/test_helper.exs", "test/test_helper.exs"},
    {:eex, "new/test/test_template.exs", "test/:app_name.exs"}
  ]

end
