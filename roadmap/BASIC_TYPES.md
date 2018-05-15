# Basic types


Because Elm and Elixir share a lot of common basic types, there is no need to redefine all of them for Elchemy. For simplicity and interoperability some of the standard types translate directly to each other.

Here is a table of all standard types used in the Elchemy environment and their Elixir equivalents:

|  Elchemy | Elixir |
|  --- |  --- |
| `a` | `any()`
| `comparable` | `term()`
| `Int` | `integer()`
| `Float` | `number()`
| `number` | `number()`
| `Bool` | `boolean()`
| `Char` | `integer()`
| `String` | `String.t()`
| `List x` | `list()`
| `(1, 2)` | `{1, 2}`
| `Maybe Int` | `{integer()}` &#124; `nil`
| `Just x` | `{x}`
| `Nothing` | `nil`
| `Result x y` | `{:ok, y}` &#124; `{:error, x}`
| `Ok x` | `{:ok, x}`
| `Err x` | `{:error, x}`
| `{x = 1, y = 1}`   | `%{x: 1, y: 1}` |
| `Dict String Int`   |  `%{key(String.t()) => integer())}` |
