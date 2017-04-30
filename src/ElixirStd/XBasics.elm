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


compare : comparable -> comparable -> Order
compare a b =
    if a > b then
        GT
    else if a < b then
        LT
    else
        EQ



-- Cant implement >> or << yet
-- typespec don't work for such types yet
{- ex

   def l >>> r do
     fn x -> l.(r.(x)) end
   end

-}


not : Bool -> Bool
not x =
    lffi "not" x


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
