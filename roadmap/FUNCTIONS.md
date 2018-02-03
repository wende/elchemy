# Function definition and currying

### Function definition
To define a function that gets exported you **need** to declare a type for it with

``` elm
functionName : ArgType -> ArgType2 -> ReturnType
```

And a function body underneath

``` elm
functionName argOne argTwo = body
```

This will output following definition:

``` elixir
curry function_name/2
@spec function_name(arg_type, arg_type2) :: return_type
def function_name(arg_one, arg_two) do
  body
end
```

Following rules apply:

1. A function will use `def` or `defp` based on `exposing` clause at the top of the module
2. The name of the function as well as its spec will always be snake_cased version of the camelCase name
3. `curry function/arity` is a construct that makes the function redefined in a form that takes 0 arguments, and returns X times curried function (2 times in this example). Which means that from elixir our function can be called both as: `function_name(arg_one, arg_two)` or `function_name().(arg_one).(arg_two)` and it won't have any different effect
4. `@spec` clause will always resolve types provided, to a most readable and still understandable by the elixir compiler form

#### Curried definition
Because of the curried nature of Elm function definitions we can just make our function return functions

For example instead of writing

``` elm
addTwo : Int -> Int
addTwo a = 2 + a
```

We could just write

``` elm
addTwo : Int -> Int
addTwo = (+) 2
```

In which case Elchemy will recognize a curried return and still provide you with a 1 and 0 arity functions, not only the 0 one.
And output of such a definition would look like:

``` elixir
curry add_two/1
@spec add_two(integer) :: integer
def add_two(x1) do
   (&+/0).(2).(x1)
end
```
Which basically means that Elchemy derives function arity from the type, rathar then function body
