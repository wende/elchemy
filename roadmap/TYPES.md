# Union types

Elchemy (exactly like Elm) uses [Tagged Union types](https://en.wikipedia.org/wiki/Tagged_union)  
What it means is basically you can define a type by adding a tag to
an already existing value and a meaning of tag is only to inform what is the
context of that value.

For instance

``` elm
type Shape = Dot Int | Line Int Int | Triangle Int Int Int
```
What's important is that Dot, Line and Triangle are just tags, so they can't be used as a type name (in function signature for example)
The only purpose of these is to pattern match on them in constructs like case..of, let..in or in arguments

Elchemy represents tagged unions as tuples with a first element being an atom with snake_cased tag name, or - in case of just tags - as a single atom value.

For example our previously defined type would translate to

``` elixir
@type shape :: { :dot, integer() } |
               { :line, integer(), integer() } |
               { :triangle, integer(), integer(), integer() }
```
But a type like this

``` elm
type Size = XS | S | M | L | XL
```
Would translate to

``` elixir
@type size :: :x | :s | :m | :l | :xl
```
### Type Parameters

Types can also take type parameters like

``` elm
type Maybe x = Just x | Nothing
```
All type parameters will resolve to any() by Elchemy

### Types as constructors

Types can be instantiated using a tag and values it takes.
For example to instantiate `Just Int` we would write `Just 10`.
If you don't provide all of the parameters, Elchemy will recognize it and
translate it into a curried function, so that `Just` instead of turning to
``` elixir
:just
```
it turns to
``` elixir
fn x1 -> {:just, x1} end
```
