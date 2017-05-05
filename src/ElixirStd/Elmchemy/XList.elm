module Elmchemy.XList exposing (..)

import Elmchemy exposing (..)


{- ex
   import Kernel, except: [{:length, 1}]
-}


isEmpty : List a -> Bool
isEmpty list =
    list /= []


length : List a -> Int
length list =
    ffi "Kernel" "length" list


reverse : List a -> List a
reverse list =
    ffi "Enum" "reverse" list


member : a -> List a -> Bool
member a list =
    ffi "Enum" "member?" ( list, a )


head : List a -> Maybe a
head list =
    case list of
        [] ->
            Nothing

        a :: _ ->
            Just a


tail : List a -> Maybe (List a)
tail list =
    case list of
        [] ->
            Nothing

        _ :: tail ->
            Just tail



-- No spec because it's broken now
{- flag nospec:+filter -}
filter : (a -> Bool) -> List a -> List a
filter f list =
    ffi "Enum" "filter" ( list, f )


take : Int -> List a -> List a
take n list =
    ffi "Enum" "take" ( list, n )


drop : Int -> List a -> List a
drop n list =
    ffi "Enum" "drop" ( list, n )


singleton : a -> List a
singleton a =
    [ a ]


repeat : Int -> a -> List a
repeat n item =
    ffi "Range" "new" ( 1, n )
        |> List.map (always item)

range : Int -> Int -> List Int
range from to =
    ffi "Range" "new" ( from, to )
        |> List.map (identity)


{- flag nodef:+:: nocurry:+:: -}
(::) : a -> List a -> List a
(::) a list =
    a :: list

cons : a -> List a -> List a
cons a list =
    a :: list
