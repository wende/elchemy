module Elchemy.XTuple
    exposing
        ( first
        , second
        , mapFirst
        , mapSecond
        )

{-| Module for tuple manipulation

@docs first, second, mapFirst, mapSecond

-}


{-| Extract the first value from a tuple.

    first (3, 4) == 3
    first ("john", "doe") == "john"

-}
first : ( a, b ) -> a
first ( fst, _ ) =
    fst


{-| Extract the second value from a tuple.

    second (3, 4) == 4
    second ("john", "doe") == "doe"

-}
second : ( a, b ) -> b
second ( _, snd ) =
    snd


{-| Transform the first value in a tuple.

    mapFirst String.reverse ("stressed", 16) == ("desserts", 16)
    mapFirst String.length  ("stressed", 16) == (8, 16)

-}
mapFirst : (a -> a1) -> ( a, b ) -> ( a1, b )
mapFirst f ( fst, snd ) =
    ( f fst, snd )


{-| Transform the second value in a tuple.

    mapSecond sqrt          ("stressed", 16) == ("stressed", 4.0)
    mapSecond (\x -> x + 1) ("stressed", 16) == ("stressed", 17)

-}
mapSecond : (b -> b1) -> ( a, b ) -> ( a, b1 )
mapSecond f ( fst, snd ) =
    ( fst, f snd )
