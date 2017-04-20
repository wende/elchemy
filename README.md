# Elmchemy [![Build Status](https://travis-ci.org/wende/elmchemy.svg?branch=master)](https://travis-ci.org/wende/elmchemy)
> Fun project of transpiler of Elm to Elixir

Test online at:
https://wende.github.io/elmchemy/stable/

Type
```
make dev
```
in terminal to start the compiler

# What is it?
Elmchemy is a project with one idea in mind: Seamless acommodation of Elm on Erlang VM.


# Contributing Guide
- Everyone is welcome to contribute
- Refer to http://bogdanp.github.io/elm-ast/example/ to have better understanding of parsed tokens.

# Targeted values:
- Fully readable and indented elixir code generated from compilation
- Seamless and stressless interop with existing Elixir code, preferably with magically working type safety
- Full integration with entire elm syntax for editors and compilers magic

# TODO (priority desc)
- [X] Saving and reading from files
- [X] Basic API (operators, functions, monadic types [Maybe, Just etc])
- [X] Simple Elixir/Erlang interop
- [ ] Compiler built-in into elixir ecosystem
