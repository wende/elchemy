# Flags

Elchemy compiler for purposes of experimenting and stretching the boundaries accepts flags.

#### DO NOT ATTEMPT TO DO THAT UNLESS YOU'RE SURE IT'S THE ONLY WAY

To pass a flag to a compiler there is a special comment syntax

```
{- flag flagname:+argument flagname2:+argument2 }
```

So far there is 4 flag types:
#### `notype:+TypeName`
Omits putting the `@type` into the compiled output code  
Used when you need type checking inside Elchemy ecosystem, without forwarding the definition into the output code.  
Example:
```elm
{- flag notype:+MyHiddenType -}
type MyHiddenType = Hidden a
```
---
#### `nodef:+functionName`
Omits entire function definition from the code output. The function `@spec` will still be produced.
Example:
```elm
{- flag nodef:+myHiddenFunction -}
myHiddenFunction = 1
```
---
#### `nospec:+functionName`
Omits function spec from the code output
Example:
```elm
{- flag nospec:+myHiddenFunction -}
myHiddenFunction : Int
```
---
#### `noverify:+functionName`
Omits function verify macro from the code output. Usable only when using for functions defined as FFI
Example:
```elm
{- flag nospec:+myHiddenFunction -}
myHiddenFunction : Int
```
