# Type Aliases

In Elchemy Type Aliases are completely virtual constructs that never make it out of the compiler.
However whenever a type alias is used throughout your code Elchemy will expand the alias and substitute with the right replacement.

For instance if we write

``` elm
type alias MyList = List Int

a : MyList
a = [1, 2, 3]
```

The Elixir output would be

``` elixir
@spec a :: list(integer())
def a(), do: [1, 2, 3]
```
With correct type resolution


## Type aliases as constructors

If a type alias represents a structure like

``` elm
type alias Human = { name : String, age : Int }
```
You can use the name of an alias as a function to quickly instantiate a struct
. For instance:

``` elm
Human "Krzysztof" 22
```
