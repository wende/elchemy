# Elchemy [![Build Status](https://travis-ci.org/wende/elchemy.svg?branch=master)](https://travis-ci.org/wende/elchemy)
> Write Elixir code using Elm-inspired syntax (elm-make compatible)

## What is it?
Elchemy (name subject to change) is a project with one idea in mind: Seamless acommodation of Elm-like language on Erlang VM.

### Elchemy is a community project and is in no way related to the core team of Elm language

## UPDATE: [You can read a tutorial on using Elchemy here](https://medium.com/@krzysztof.wende/elchemy-write-type-safe-elixir-code-with-elms-syntax-part-1-introduction-8968b76d721d)

## Web demo
You can test `elchemy` online at:
https://wende.github.io/elchemy/stable/

### CAVEAT: Web version does *not* do any type checking. It only parses syntax. For full type and syntax checking run the CLI version

## Usage

### Prerequisites
- [node](https://nodejs.org/en/)
- [elm-lang](https://guide.elm-lang.org/install.html)

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

## Contributing Guide
- Everyone is welcome to contribute
- Refer to http://bogdanp.github.io/elm-ast/example/ to have better understanding of parsed tokens.

## Targeted values:
- Fully readable and indented elixir code generated from compilation
- Seamless and stressless interop with existing Elixir code, preferably with magically working type safety
- Full integration with entire elm syntax for editors and compilers magic

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

## Will my employer notice I'm having an affair with Elm?
The output files of Elchemy treat the code readability as a first class citizen. The code is meant to be properly indented, the comments aren't omitted, and the code is optimized as hard as it can ( f.i case clauses reduce to function overloads)
Also auto-generated typespecs are on their way.
You might need some configuration files to compile your project, but if you don't do `git add --all` before every commit you'll be fine.

## When will Elchemy become 1.0.0?
The answer is 42

## Can I contribute?
Definitely. Yes. Please do.

## How are types represented?
You're a nosy one, aren't you?
Elchemy represents all type constructors as snake cased atoms, and all type applications as tuples.
Which means that `MyType 42 "Fourty two" Error` in Elm equals to `{:my_type, 42, "Fourty Two", :error}` in Elixir.
What means there's an additional premise to have in mind that even though in Elm `MyType 1 == (MyType, 1)` are two completely different things, in Elchemy they're not. They're exactly the same. Which is `{:my_type, 1}`. You shouldn't be using that in a code though since it's considered bad design

## Can I use already existing Elm libraries with Elchemy.
Not yet. You will be able to use Elm libraries that don't have native modules or ports

## Can I use already existing Elixir libraries with Elchemy
Yes. You can do an `ffi` call to any function in any module. Whether it's Elixir module, Erlang module, or even a macro you can include it in your code. Just remember to wrap them in as small chunks as possible to not sacrifice any type safety.

## But what about out of function macros? Like tests and `use Module`?
Unfortunatelly you can't write any macros with `do..end` blocks yet. You can write any out of function oneliners using Module meta feature:
```elm
meta = 
  [ "use GenServer" ]
```

## Can I define an Elixir macro in Elchemy.
So you want to write an Elm-like code, that will manipulate Elixir code, which generates and Elixir code that manipulates Elixir code? How about no?

## Do I need to have Elm installed to compile my `.elm` files with Elchemy.
Ironically, no, you don't. Elchemy is written entirely in Elm, which means the executable file of the compiler is in JavaScript.
But it's nice to use `elm-make` to check your type safety from time to time.


# Progress of the project

- Parser - **98%** (of Elm's syntax)
- Compiler - **90%** (still needs some nicer solutions of code generation plus we might come out with some new nice ideas)
- Elchemy-core - **40%** ( We have Basics, Debug, Char, String, List, Result, Tuple and Maybe) 
- Elchemy-core for Erlang VM - **0%** (Everything for os related tasks like filesystem, OTP goodies etc has to be done)
- Elchemy-effects - **0%** - You can't and shouldn't write anything with side-effects in Elchemy yet
- Documentation - **30%** - There's couple of tutorials online and exemplar project. Nothing fancy yet, though
- Interop with Elixir - **50%** - It works and it's semi-typesafe. But reliability of its type safetyness is questionable.
- Ideology - **30%** - We only have a gist. There is so much work to do we rarely have time to sit and just think
