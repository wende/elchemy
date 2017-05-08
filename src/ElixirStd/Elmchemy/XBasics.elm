module Elmchemy.XBasics exposing (..)

import Elmchemy exposing (..)


type Order
    = LT
    | EQ
    | GT



-- Operators
{- ex

   import Kernel, except: [
     {:'++', 2},
     {:round, 1},
     {:to_string, 1}

   ]

   curry ==/2
   curry !=/2
   curry </2
   curry >/2
   curry <=/2
   curry >=/2
   curry max/2
   curry min/2

   curry &&/2
   curry ||/2

   curry +/2
   curry -/2
   curry */2
   curry //2
   curry div/2
   curry rem/2
   curry abs/1
   # Inlined from not
   curry !/1

-}


{-| Basic compare function


### Example

    compare 1 2 == LT

-}
compare : comparable -> comparable -> Order
compare a b =
    if a > b then
        GT
    else if a < b then
        LT
    else
        EQ



{- ex
   # >> is replaced with >>> by the compiler
   def l >>> r do
     fn x -> r.(l.(x)) end
   end

-}
-- not/1 is inlined by the compiler


xor : Bool -> Bool -> Bool
xor a b =
    (a && not b) || (not a && b)


negate : number -> number
negate x =
    lffi "-" x


sqrt : number -> Float
sqrt x =
    ffi ":math" "sqrt" x


clamp : comparable -> comparable -> comparable -> comparable
clamp x bottom top =
    x
        |> min bottom
        |> max top


logBase : Float -> Float -> Float
logBase a b =
    notImplemented


e : Float
e =
    2.71828


pi : Float
pi =
    lffi "apply" ( ":math", "pi", [] )


cos : Float -> Float
cos x =
    ffi ":math" "cos" x


sin : Float -> Float
sin x =
    ffi ":math" "sin" x


tan : Float -> Float
tan x =
    ffi ":math" "tan" x


acos : Float -> Float
acos x =
    ffi ":math" "acos" x


asin : Float -> Float
asin x =
    ffi ":math" "asin" x


atan : Float -> Float
atan x =
    ffi ":math" "atan" x


atan2 : Float -> Float -> Float
atan2 x y =
    ffi ":math" "atan2" ( x, y )


round : Float -> Int
round x =
    lffi "round" x


floor : Float -> Int
floor x =
    notImplemented


ceiling : Float -> Int
ceiling x =
    notImplemented


truncate : Float -> Int
truncate x =
    notImplemented


toFloat : Int -> Float
toFloat x =
    ffi "Kernel" "*" ( x, 1.0 )


toString : a -> String
toString x =
    ffi "Kernel" "to_string" x


(++) : appendable -> appendable -> appendable
(++) a b =
    if lffi "is_binary" a && lffi "is_binary" b then
        ffi "Kernel" "<>" ( a, b )
    else
        ffi "Kernel" "++" ( a, b )


identity : a -> a
identity a =
    a


id : a -> a
id a =
    identity a


always : a -> a -> a
always a b =
    a

{- flag nospec:+flip -}
flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a

-- TODO Will be fixed with #34
{- ex
   @spec curried(({any, any} -> any)) :: ((any -> any) -> any)
   curry curried/1
   def curried(fun) do
     fn fst -> fn snd -> fun.({fst, snd}) end end
   end

   @spec uncurried(((any -> any) -> any)) :: ({any, any} -> any)
   curry uncurried/1
   def uncurried(fun) do
     fn {fst, snd} -> fun.(fst).(snd) end
   end

-}
-- We don't care for Never type

-- Additional

notImplemented : a
notImplemented =
    lffi "throw" "Not implemented"

tuple2 : a -> b -> (a, b)
tuple2 a b =
    (a, b)

tuple3 : a -> b -> c -> (a, b, c)
tuple3 a b c =
    (a, b, c)

tuple4 : a -> b -> c -> d -> (a, b, c, d)
tuple4 a b c d =
    (a, b, c, d)

tuple5 : a -> b -> c -> d -> e -> (a, b, c, d, e)
tuple5 a b c d e =
    (a, b, c, d, e)
