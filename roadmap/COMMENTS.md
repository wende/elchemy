# Comments

In Elchemy there is several types of comments.

### Inline comments
```elm
-- My comment
```

Which are always ignored by the compiler and won't be included in the compiled output file

### Block comment

Second type of a comment is a block comment

``` elm
{- block comment -}
```

Which are included in the output file, but can only be used as a top level construct

### Doc comments
Another one and probably most important is a doc-comment.

``` elm
{-| Doc comment -}
```
Doc-comments follow these rules:

1. **First doc comment in a file always compiles to a `@moduledoc`**
2. Doc comments before types compile to `@typedoc`
3. Doc comments before functions compile to `@doc`
4. A doc comment with 4 space indentation and containing a `==` comparison in it will be compiled to a **one line** [doctest](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html#doctests)

Doctest example:

```elm
{-| My function adds two values

       myFunction 1 2 == 3
-}
```

Would end up being

```elixir
@doc """
     My function adds two values

        iex> my_function().(1).(2)
        3
 """
```
