# Structs

Elchemy represents all the structs as maps, so a struct defined like

``` elm
human : { name : String
        , age : Int
        }
```
Is an equivalent of

``` elixir
@spec human :: %{name: String.t(), age: integer()}
```

Also type aliases denoting structs can be instantiated like functions

``` elm
type alias Human =
     { name : String
     , age : Int
     }
```
``` elm
Human "Krzysztof" 22
```

## Struct polymorphism
What's more structs can describe a map that has at least specified elements using an update syntax.

``` elm
type alias Employee x =
     { x
     | salary : Int
     }
```
Which means any struct that has a field `salary` of type integer.
That way we can define our pseudo-inheritance and polymorphism for more advanced structures.

``` elm
type alias Human =
     Employee
        { name : String
        , age : Int }

human : Human
```
Would resolve to

``` elixir
@spec human :: %{
  salary: integer(),
  name: String.t,
  age: integer()
}
```

But be advised that using this "polymorphic" approach strips us from the ability to use type aliases as constructors.
