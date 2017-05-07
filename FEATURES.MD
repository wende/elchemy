
| Elixir | Elm | Elmchemy | implemented? |
| --- | --- | --- | --- |
| var | let in | let in | yes |
atom | none | Types constructors -> atoms | yes
none | type | type | yes
list | list | list | yes
none | Maybe | Maybe | yes
none | Just a | Just a > {a} | yes
nil | Nothing | Nothing -> nil | yes
fn/0 | none | none | not planned 
fn/1 | \\a | \\a | yes 
fn/x | \\a b... | lambdas translate to curried fn/1. Other lambdas can be expressed with (flambda arity f) | yes
tuple | tuples | tuples with first atom == types with params & normal tuples | yes
charlist | String | none | TBD
binary (string)  | none | String | yes
One element charlist | Char | Char | yes
integer | Int | Int | yes
float | Float | Float | yes
number | number | number | yes
none | comparable | comparable = term | 1/2
| 0 arity fun | same | same | yes 
\> 0 arity fun | curried fun | curried & multi arity funs with one definition | yes
@spec | type definition | type definition compiles to typspec | yes but buggy 
tests | tests | - - | planned 
doctest | no | one line doctest | yes
@doc | {-\| -} | {-\| -} | yes 
destructuring in function head | same | same | yes
destructuring asignment | let in destruct | none | to be implemented 
destructuring in case | same | same | yes
asignment in destructurng | as | as | in case expression only
