# Elmchemy [![Build Status](https://travis-ci.org/wende/elmchemy.svg?branch=master)](https://travis-ci.org/wende/elmchemy)
Fun project of transpiler of Elm to Elixir

Test online at:
https://wende.github.io/elmchemy/

Type
```
make dev
```
in terminal to start the compiler

# What is it?
Example of a transiled code using this transpiler:
```elm
module Main exposing (..)
import Elixir.Glue exposing (..)
f : Int -> Int
f x = x + 1
add : Int -> Int -> Int
add a b = a + Just b
h = wende g + hajto cichocinski 10
casa t =
    case t of
        a -> 1
        b ->
            \a -> 1 + 2
-- If you alias a record, you can use the name as a constructor function.
otherOrigin : Point3D
otherOrigin =
  Point3D 0 0 0
```
Produces:
```elixir
use Elmchemist.Glue
defmodule Main do
  import Elixir.Glue
  @spec f(int()) :: int()
  defun f(x) do
    x + 1
  end

  @spec add(int(), int()) :: int()
  defun add(a,b) do
    a + {:ok, b}
  end

  defun h() do
    wende.(g) + hajto.(cichocinski), 10
  end

  defun casa(t) do
    case t do
      wende -> 1
      cichocinski -> fn(a) -> 1 + 2 end
    end
  end

  #  If you alias a record, you can use the name as a constructor function.
  @spec otherOrigin :: Point3D
  defun otherOrigin() do
    {:point3d, 0, 0, 0}
  end
end
```

# Targeted values:
- Fully readable and indented elixir code generated from compilation
- Seamless and stressless interop with existing Elixir code, preferably with magically workng type safety
- Full integration with entire elm syntax for editors and compilers magic

# TODO (priority desc)
- [X] Saving and reading from files
- [ ] Basic API (operators, functions, monadic types [Maybe, Just etc])
- [ ] Compiler built-in into elixir ecosystem
- [ ] Glue code for Elmchemist interop
- [ ] Glue code for Elixir interop
