# Module definition and imports

To define a module in Elchemy you need to use a

``` elm
module ModuleName exposing (..)
```

Which would directly translate to `defmodule` block where functions/types mentioned in the `exposing` clause will automatically use `def` or `defp`

## Imports

There are two types of imports in Elchemy.

### Without exposing
One is a import without exposed functions like

``` elm
import SomeModule
```
Which would directly translate to

``` elixir
alias SomeModule
```

Because it doesn't import any of the exposed contents, only makes sure
that the module is in our namespace.

### With exposing

``` elm
import SomeModule exposing (funA, TypeA, funB)
```
Which outputs

``` elixir
import SomeModule, only: [{:fun_a, 0}, {:fun_b, 0}]
```

Which would put `SomeModule` into our namespace and also allow us to use
`fun_a` and `fun_b` without explicitly adding a module name before them.
