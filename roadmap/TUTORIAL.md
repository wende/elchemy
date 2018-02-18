# An Elchemy tutorial: write type-safe Elixir code with Elm's syntax

## Few words of introduction

We all love Elixir. It's expressive, it's extremely readable and - first and foremost - It runs on BEAM — an incredible virtual machine that turns concurrency into something as natural and expected as NullPointerExceptions in Java 7.

But as every language does, it also has its flaws. Some are inherent, some are just religious choices that had to be made one way or another; and as the user (or rather fan) base grows, so does the demand to express your own preferences. Changes like piping to the last argument instead of the first can be introduced with simple macros. Some people craving for more advanced ideologies create entire toolsets extending the language (like [Railway Oriented Programming](http://www.zohaib.me/railway-programming-pattern-in-elixir/) or [Saša Jurić's Exactor](https://github.com/sasa1977/exactor)). Some others commit to entire frameworks that start to look like DSLs instead ([Witchcraft](https://github.com/expede/witchcraft/)).
But even smartest macro can't solve everything without surpassing the biggest meta-programmer's limitation: syntax. So that's what we changed.

Arguably one of the loudest religion choices is typing. Private jokes like "Strong typing is for weak minds" and "If you shoot yourself in the foot, it's easier to solve the problem by being careful not to aim the gun at your foot than it is to make guns that don't point down" roam around the community. Whether types are hot or not, is out of this article's scope.
I like strong types. And I like Elixir.
And that's why __Elchemy__ was made.

## What does strong typing give us anyway?

Generally strong typing pays attention so you don’t have to. 
You put a typo in function params?
 Compiler will tell
Your arguments are in wrong order.
 Compiler will tell
You wrongly treat a list of `{:ok, value}` tuples as if they were just `value`’s.
 Compiler will tell.

So whether your application works on complex nested structures, you like extra safety, or you just suffer from short attention span, static typing probably will serve you well.

## Elchemy to the rescue!

With all of those values in mind, we came out with [Elchemy](https://github.com/wende/elmchemy).

Without further and unnecessary descriptions. Elchemy is entirely about turning this:

```elm
module FizzBuzz exposing (fizzbuzz)

import List exposing (map, range)

{-| Fizzes the buzzes and buzzfizzes the fizz out of buzz
    fizzbuzz 1 7 == "1 2 Fizz 4 Buzz Fizz 7"
-}
fizzbuzz : Int -> Int -> String
fizzbuzz from to = 
  let fizzBuzz n = case (n % 3, n % 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _      -> toString n
  in List.range from to |> map (fizzBuzz >> toString) |> joinWords
  
  
joinWords : List String -> String
joinWords list = String.join " " list
```

Into this:

```elixir
defmodule FizzBuzz do
  use Elmchemy

  import XList, only: [{:'range', 0},{:'map', 0}]
  @doc """
  Fizzes the buzzes and buzzfizzes the fizz out of buzz
      iex> import FizzBuzz
      iex> fizzbuzz.(1).(7)
      "1 2 Fizz 4 Buzz Fizz 7"
  """
  @spec fizzbuzz() :: (integer -> (integer -> String.t))
  @spec fizzbuzz(integer, integer) :: String.t
  curry fizzbuzz/2
  def fizzbuzz(from, to) do
    fizz_buzz = fn(n) -> case {rem(n, 3), rem(n, 5)} do
      {0, 0} -> "FizzBuzz"
      {0, _} -> "Fizz"
      {_, 0} -> "Buzz"
      _ -> to_string.(n)
    end end
    XList.range.(from).(to)
    |> (map.(fizz_buzz >>> to_string)).()
    |> (join_words).()
  end

  @spec join_words() :: (list(String.t) -> String.t)
  @spec join_words(list(String.t)) :: String.t
  defp join_words(list) do
    XString.join.(" ").(list)
  end

end
```

Does it work? [Check by yourself](https://wende.github.io/elchemy/stable/).


> "But wait. I just typed

> a : String
> a = 1

> And it totally went through. Where is the entire type safety in that? You suck!"

Worry not. The type safety’s there. Just not in the browser, but living safely in the depths of the entire toolkit to integrate Elchemy with your existing Elixir project.

And to prove and learn the basics of it, below we’ll write a simple example program from scratch.

## Example program

![elchemyinit](elchemy_init.svg)

We don't have any project so we’ll start a new one:

    $ mix new article_example_elchemy
    $ cd article_example_elchemy

Now we also need to install Elchemy and initialize it in our project
So let's do that and try out how our type safety is doing.

    $ npm install -g elchemy
    $ elchemy init

Terminal responds to us with:

```
Elchemy 0.6.4 initialised. Make sure to add:

        |> Code.eval_file("elchemy.exs").init

to your mix.exs file as the last line of the project() function.
This pipes the project keyword list to the elchemy init function to configure some additional values.

Then run mix test to check if everything went fine
```

Open your favorite editor and edit the file as asked. You should have something along the lines (I'm using `elixir 1.6.0`):

```elixir

defmodule ArticleExampleElchemy.MixProject do
  use Mix.Project

  def project do
    [
      app: :article_example_elchemy,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
      |> Code.eval_file("elchemy.exs").init
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
    ]
  end
end
```

Great! We’re good to go. Let’s type `mix test` to make sure that we’re all set.

We should see a ton of logs and warnings. But don’t worry — as long as the tests are passing everything’s fine!
