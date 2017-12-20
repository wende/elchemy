
<p align="center">
<img src="https://github.com/wende/elchemy/blob/master/logo.png?raw=true" width="250" height="250">
</p>
<p align="center">
  <a href="https://gitter.im/elchemy-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge">
    <img src="https://badges.gitter.im/elchemy-lang/Lobby.svg">
  </a>
  <a href="https://travis-ci.org/wende/elchemy">
    <img src="https://travis-ci.org/wende/elchemy.svg?branch=master">
  </a>
</p>

#### Quick install
```shell
npm install -g elchemy
```

# What is it?
Elchemy lets you write simple, fast and quality type safe code while leveraging both the Elm's safety and Elixir's ecosystem

## [You can read a tutorial on using Elchemy here](https://medium.com/@krzysztof.wende/elmchemy-write-type-safe-elixir-code-with-elms-syntax-part-1-introduction-8968b76d721d)
## [You can read Elchemy documentation (WIP) here](https://wende.github.io/elchemy-ivy/)
## [You can test Elchemy online here](https://wende.github.io/elchemy/)
### CAVEAT: Web version does *not* do any type checking. It only parses syntax. For full type and syntax checking run the CLI version
### In case of any questions about the project feel free to submit them in Issues with Q&A label

# Features
- **Type inference:** Powerful type inference means you rarely have to annotate types. Everything gets checked for you by the compiler
- **Easy and type-safe interop**: You can call Elixir/Erlang without any extra boiler-plate. All the calls you make are checked in terms of type-safety as thoroughly as possible
- **All the best of Elm and Elixir**: Elchemy inherits what's best in Elm - type safety, inference and extreme expressiveness, but also what's best in Elixir - Doc-tests, tooling and obviously the entire BEAM platform.
- **Nearly No runtime errors** - Elchemy's type system **eliminates almost all runtime errors**. With a shrinking set of edge cases, your entire app will be as safe as the parts written in Elixir are.
- **Beatiful and fully readable output** - All of the code produced by Elchemy can be easily read and analyzed without taking a single look at the source code

# Patch Notes:
### v0.5
  - Numerous output code quality and performance improvements
  - Functions can be shadowed (Elm behaviour over Elixir's behaviour)
  - Support for Elixir 1.5
  - Elchemy can compile itself without any parsing or compiling errors. (2 out of 3 steps complete)
### v0.4 
  - New name `Elchemy` (without 'm')
    - New repository and std lib name 
    - New compile tools commands (All commands can be still called with an old name and it will be supported for next couple of versions)
### v0.3
  - Native modules
  - Imported types and type aliases
  - Type polimorhpism 
  - Advanced syntax and bugfixes


# Usage

### Prerequisites
- [node@5+](https://nodejs.org/en/)
- [elm-lang@0.18](https://guide.elm-lang.org/install.html)
- [elm-github-install@0.1.2](https://github.com/gdotdesign/elm-github-install) - Compiler will install it automatically for you, if you don't have it yet

### Installation in Elixir project
Install `elchemy` globally with

```shell
npm install -g elchemy
```

Then in root of your project do:
```shell
elchemy init
```

And follow the instructions

`elchemy` will find all `*.elm` files specified in `elchemy_path` and compile it into corresponding `*.ex` files in `lib` directory.

You can override output directory specifing `elixirc_paths`.

### Instalation as a standalone
```shell
npm install -g elchemy
```
Usage
```
elchemy compile source_dir output_dir
```
### Build from source

```
git clone https://github.com/wende/elchemy.git
cd elchemy
make compile
./elchemy compile source_dir output_dir
```
and 
```
make dev
```
To launch and test the web demo

# Contributing Guide
- Everyone is welcome to contribute
- Refer to http://bogdanp.github.io/elm-ast/example/ to have better understanding of parsed tokens.
- For project management we use ZenHub. You can see the Kanban board, card estimates and all the reports by installing a browser extension here: [Opera/Chrome](https://chrome.google.com/webstore/detail/zenhub-for-github/ogcgkffhplmphkaahpmffcafajaocjbd), [Firefox](zenhub.com)

## Targeted values:
- Fully readable and indented elixir code generated from compilation
- Seamless and stressless interop with existing Elixir code, preferably with magically working type safety
- Full integration with entire elm syntax for editors and compilers magic

## Troubleshooting
If anything doesn't work, try 
```
npm install -g elchemy
elchemy clean
elchemy init
mix test
```
first

# FAQ
## Why *would* I want to use that
- You like types
- But even more you prefer compile-time errors over run-time error
- You prefer `add b c = b + c` over `defp add(a, b), do: b + c`
- You like curry
- You think failing fast is cool, but not as cool as not failing at all

## Why *wouldn't* I want to use that
- Your project relies on die-hard battle tested libraries, and you despise any versions starting with 0
- You're afraid that when you learn what Monad is your mustache will grow, and eyesight weaken

## Can I use it in already existing Elixir project
You can, but nice and dandy compile tools are still on their way

## Will my employer notice I'm having an affair with Elchemy?
The output files of Elchemy treat the code readability as a first class citizen. The code is meant to be properly indented, the comments aren't omitted, and the code is optimized as hard as it can ( f.i case clauses reduce to function overloads)

## When will Elchemy become 1.0.0?
When it's done

## Can I contribute?
Definitely. Yes. Please do.

## How are types represented?
You're a nosy one, aren't you?
Elchemy represents all type constructors as snake cased atoms, and all type applications as tuples.
Which means that `MyType 42 "Fourty two" Error` in Elchemy equals to `{:my_type, 42, "Fourty Two", :error}` in Elixir.

## Can I use already existing Elm libraries with Elchemy.
As long as they don't use any Native modules, Ports or Elm runtime they can be safely imported and used

## Can I use already existing Elixir libraries with Elchemy
Yes. You can do an `ffi` call to any function in any module. Whether it's Elixir module, Erlang module, or even a macro you can include it in your code. Ffi calls are a treated specially in Elchemy and they get generated test to analyze the types based on @specs, so that you don't compromise type safety for using Elixir code. 
In order to increase readbility it's advised no to use `ffi` calls if not necessary and always document and doctest them.

## But what about out of function macros? Like tests and `use Module`?
Unfortunatelly you can't write any macros with `do..end` blocks yet. You can write any out of function code using an elixir inline code with:
```elm
{- ex
  code_here
-}
```
But it's a last resort solution and shouldn't ever be abused.

## Can I define an Elixir macro in Elchemy.
So you want to write an Elm-like code, that will manipulate Elixir code, which generates an Elixir code that manipulates Elixir code? How about no?

## Do I need to have Elm installed to compile my `.elm` files with Elchemy.
Elchemy uses Elm to typecheck your program. Although it is possible to use it without Elm on your machine it's not advised.


# Progress of the project

- Parser - **98%** of Elm's syntax
- Compiler - **97%** (still needs some nicer solutions of code generation plus we might come out with some new nice ideas)
- Elchemy-core - **93** ( We have Basics, Debug, Char, String, List, Result, Tuple, Set, Bitwise, Dict and Maybe) 
- Interop with Elixir - **90%** - All of the interop is mature and type-safe-verified based on specs 
- Ideology - **60%** - We've got a pretty solid idea of where Elchemy is going 
- Documentation - **50%** - There's couple of tutorials online and example projects. Nothing fancy yet, though
- Elchemy-effects - **15%** - You can't and shouldn't write anything with side-effects in Elchemy yet. We're working on finding the best solution for effects that would fit both Elm's and Elixir's community
- Elchemy-core for Erlang VM - **5%** (Everything for os related tasks like filesystem, OTP goodies etc has to be done)

