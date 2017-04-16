defmodule Elmchemy.Test do
  require Elmchemy.Glue
  import Elmchemy.Glue


  defcurry add(a, b) do
    notadd().(a).(b)
  end

  defcurryp notadd(a, b) do
    a + b
  end

end
