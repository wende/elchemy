defmodule <%= app_module %>Test do
   use ExUnit.Case
   use Elchemy

   doctest <%= app_module %>
   typetest <%= app_module %>

    test do
      assert <%= app_module %>.hello == "world"
    end
end