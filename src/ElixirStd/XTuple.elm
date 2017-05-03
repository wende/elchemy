module XTuple exposing (..)


first : ( a, b ) -> a
first ( fst, _ ) =
    fst


second : ( a, b ) -> b
second ( _, snd ) =
    snd


mapFirst : (a -> a1) -> ( a, b ) -> ( a1, b )
mapFirst fn ( fst, snd ) =
    ( fn fst, snd )


mapSecond : (b -> b1) -> ( a, b ) -> ( a, b1 )
mapSecond fn ( fst, snd ) =
    ( fst, fn snd )
