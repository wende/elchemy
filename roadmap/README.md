# Elchemy

This page lists all of the ideas and solutions behind Elchemy to share the
ideology as well as existing and incoming solutions to problems this project faced
or eventually will have to face

# Table of contents
## Basics
  - [Basic Types](BASIC_TYPES.md)
  - [Defining Types](TYPES.md)
  - [Defining Type Aliases](TYPE_ALIASES.md)
  - [Structures](STRUCTURES.md)
  - [Defining Modules](MODULES.md)
  - [Defining Functions](FUNCTIONS.md)
  - [Comments](COMMENTS.md)
  - [Interop (Foreign Function Interface)](INTEROP.md)

## Advanced
  - [Side Effects / TEA](SIDE_EFFECTS.md)
  - [Unit Testing](TESTING.md)
  - [Compiler flags](FLAGS.md)
  - [Inlining Elixir](INLINING.md)

## Tooling
  - [Installation](./INSTALLATION.md)
  - [Troubleshooting](TROUBLESHOOTING.md)

# Introduction


Elchemy is a set of tools and frameworks, designed to provide a language and an environment
as close to [Elm programming language](http://elm-lang.org) as possible, to build server applications
in a DSL-like manner for Erlang VM platform, with a readable and efficient Elixir code as an output.

## Features

Elchemy inherits many values from its parents: Elm and Elixir

### Elm
- ML like syntax maximizing expressiveness with additional readability and simplicity constraints
- Static typing with type inference
- Beautiful compilation errors
- Tagged union types and type aliases with type parameters (aka generic types)
- All functions are curried by default
- [No typeclasses](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)

### Erlang/Elixir
- Documentation as a first class citizen
- Doc tests
- Battle-tested distribution system that just works

### Additional
- Foreign function calls type safety
- Foreign function calls purity checks
- Dependency system based on GitHub
- Compile time code optimizations
