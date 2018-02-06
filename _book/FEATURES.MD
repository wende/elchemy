
| Elixir | Elm | Elchemy | implemented? |
| --- | --- | --- | --- |
var | let in | let in | yes
atom | none | Types constructors -> atoms | yes
none | type | type | yes
list | list | list | yes
none | Maybe | Maybe | yes
none | Just a | Just a > {a} | yes
nil | Nothing | Nothing -> nil | yes
fn/0 | none | `flambda 0 value` | yes
fn/1 | \\a | \\a | yes 
fn/x | \\a b... | lambdas translate to curried fn/1. Other lambdas will be automatically uncurried in ffi/Native calls
tuples | tuples | tuples | yes
charlist | none | none | TBD
binary (string)  | String | String | yes
One element charlist | Char | Char | yes
integer | Int | Int | yes
float | Float | Float | yes
number | number | number | yes
none | comparable | comparable = any (because elixir can compare any primitive | yes
0 arity fun | same | same | yes 
\> 0 arity fun | curried fun | curried & multi arity funs with one definition | yes
@spec | function type definition | function type definition compiles to typspec | yes
tests | tests | - - | TBD 
doctest | no | doctest | yes, but only oneliners for now
@doc | {-\| -} | {-\| -} | yes 
destructuring in function head | same | same | yes
destructuring asignment | let..in destruct | none | to be implemented 
destructuring in case | same | same | yes
asignment in destructurng | as | as | in case expression only
nested tuples | nested tuples | nested tuples | unreliable
nested lists | nested lists | nested lists | yes
tuples in lists | same | same | yes
lists in tuples | same | same | yes
String interpolation | none | Same as elixir `"value:#{val}"` | yes
receive | none | none | #41
send | none | none | TBD

