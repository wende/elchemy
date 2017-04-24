module XBasics exposing (..)


type Order
    = LT
    | EQ
    | GT


compare : comparable -> comparable -> Order
compare a b =
    if a > b then
        GT
    else if a < b then
        LT
    else
        EQ
