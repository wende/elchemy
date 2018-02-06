# Interop


## Calling Elchemy

To call generated code from Elchemy you don't need anything special.  
Each function exposed from a module can be called either in it's regular or curried form.

Keep in mind that in case of functions expecting a [higher-order function\[s\]](https://en.wikipedia.org/wiki/Higher-order_function) in its parameters Elchemy will also do it's magic to make sure it's compatible both ways.

For instance if your function looks like that:
```elm
applyFunction2 : (a -> b -> c) -> a -> b -> c
applyFunction2 f p1 p2 = f p1 p2
```

You can call it Elixir way:  
```elixir
apply_function2(fn a, b -> a + b end)
```
As well as Elchemy (curried) way:
```elixir
apply_function2(fn a -> fn b -> a + b end end)
```
And it will work as fine


---
## Calling Elixir


To call elixir from Elchemy you need to define a foreing function interface.
To do that you can use `ffi` [special syntax (?)](SPECIAL_SYNTAX.md)

`ffi` requires the function to have a very specific format, which is:

1. You need to make sure the type signature is adequate to the corresponding typespec of a function
2. There should be no explicitly stated parameters (Defined as `f = ...` not `f a b c = ...`)
3. The **only** expression inside the function should be an ffi call in a format of:
`ffi "Module" "function"`

A good example of an ffi call would be

```elm
    upcase : String -> String
    upcase =
        ffi "String" "upcase"
```
A generated code of that statement would be

``` elixir
    @spec upcase(String.t) :: String.t
    curry upcase/1
    verify as: String.upcase/1
    def upcase(a1), do: String.upcase(a1)
```

Where `verify, as:` is a type safety generator about which you can read more in [testing](TESTING.md) section.
