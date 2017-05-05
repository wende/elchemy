module XString exposing (..)

import Elmchemy exposing (..)


-- isEmpty


isEmpty : String -> Bool
isEmpty str =
    "" == str



--cons


cons : Char -> String -> String
cons c str =
    ffi "Kernel" "to_string" c ++ str



-- fromChar
-- uncons


uncons : String -> Maybe ( Char, String )
uncons str =
    Nothing



-- append : should be <> instead of ++


append : String -> String -> String
append a b =
    a ++ b



-- concat


concat : List String -> String
concat list =
    ""



-- length


length : String -> Int
length str =
    ffi "String" "length" str



-- map
-- filter
-- reverse
-- foldl
-- foldr
-- split
-- join
-- slice
-- left
-- right
-- dropLeft
-- dropRight
-- pad
-- padLeft
-- padRight
-- trim
-- trimLeft
-- trimRight
-- words


words : String -> List String
words str =
    ffi "String" "split" str



-- lines


lines : String -> List String
lines str =
    ffi "String" "split" ( str, [ "\x0D\n", "\x0D", "\n" ] )



-- toUpper


toUpper : String -> String
toUpper str =
    ffi "String" "upcase" str



-- toLower


toLower : String -> String
toLower str =
    ffi "String" "downcase" str



-- any
-- all
-- contains


contains : String -> String -> Bool
contains contents str =
    ffi "String" "contains?" ( str, contents )



-- startWith


startsWith : String -> String -> Bool
startsWith prefix str =
    ffi "String" "start_with?" ( str, prefix )



-- endsWith


endsWith : String -> String -> Bool
endsWith suffix str =
    ffi "String" "ends_with?" ( str, suffix )



-- indexes
-- indices
-- toInt
-- toFloat
-- toList
-- fromList
