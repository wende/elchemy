module Elmchemy.XBasics exposing (..)

import Elmchemy exposing (..)
import XList


type Order
    = LT
    | EQ
    | GT



-- Operators
{- ex

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
   curry abs/2

-}


{-| Basic compare function


### Example

    compare a b

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
     fn x -> l.(r.(x)) end
   end

-}
-- not/1 is inlined by the compiler


xor : Bool -> Bool -> Bool
xor a b =
    (a && not b) || (not a && b)

negate : number -> number
negate x =
    lffi "-" x


sqrt : Float -> Float
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
    ffi ":math" "atan2" x


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
    ffi "Elmchemy" "*" ( x, 1.0 )


toString : a -> String
toString x =
    lffi "to_string" x


(++) : appendable -> appendable -> appendable
(++) a b =
    if lffi "is_string" a && lffi "is_string" b then
        ffi "Elmchemy" "<>" ( a, b )
    else
        ffi "Elmchemy" "++" ( a, b )


indentity : a -> a
indentity a =
    a


id : a -> a
id =
    identity


always : a -> a -> a
always a =
    \_ -> a


(|>) : a -> (a -> b) -> b
(|>) left fun =
    ffi "Kernel" "|>" ( left, fun )



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


notImplemented : a
notImplemented =
    lffi "throw" "Not implemented"
