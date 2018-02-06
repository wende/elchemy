module Elchemy.XString
    exposing
        ( isEmpty
        , length
        , reverse
        , repeat
        , cons
        , uncons
        , fromChar
        , append
        , concat
        , split
        , join
        , words
        , lines
        , slice
        , left
        , right
        , dropLeft
        , dropRight
        , contains
        , startsWith
        , endsWith
        , indexes
        , indices
        , toInt
        , toFloat
        , toList
        , fromList
        , toUpper
        , toLower
        , pad
        , padLeft
        , padRight
        , trim
        , trimLeft
        , trimRight
        , map
        , filter
        , foldl
        , foldr
        , any
        , all
        )

{-| A built-in representation for efficient string manipulation. String literals
are enclosed in `"double quotes"`. Strings are *not* lists of characters.


# Basics

@docs isEmpty, length, reverse, repeat


# Building and Splitting

@docs cons, uncons, fromChar, append, concat, split, join, words, lines


# Get Substrings

@docs slice, left, right, dropLeft, dropRight


# Check for Substrings

@docs contains, startsWith, endsWith, indexes, indices


# Conversions

@docs toInt, toFloat, toList, fromList


# Formatting

Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight


# Higher-Order Functions

@docs map, filter, foldl, foldr, any, all

-}

import Elchemy exposing (..)
import Elchemy.XList as XList
import Elchemy.XTuple as XTuple


{- ex
   import Kernel, except: [
     {:length, 1},
     {:'++', 2},
     {:to_charlist, 1}

   ]
   import Elchemy.XBasics, except: [
     {:to_float, 1},
    ]
-}


{-| Determine if a string is empty.

    isEmpty "" == True
    isEmpty "the world" == False

-}
isEmpty : String -> Bool
isEmpty str =
    length str == 0


{-| Add a character to the beginning of a string.

    XString.cons 'T' "he truth is out there" == "The truth is out there"

-}
cons : Char -> String -> String
cons c str =
    fromChar c ++ str


{-| Create a string from a given character.

    fromChar 'a' == "a"

-}
fromChar : Char -> String
fromChar char =
    List.singleton char |> fromCharlist


fromCharlist : List Char -> String
fromCharlist =
    ffi ":binary" "list_to_bin"


{-| Split a non-empty string into its head and tail. This lets you
pattern match on strings exactly as you would with lists.

    uncons "abc" == Just ('a',"bc")
    uncons ""    == Nothing

-}
uncons : String -> Maybe ( Char, String )
uncons str =
    let
        ( first, rest ) =
            (splitAt_ str 1)

        realFirst =
            first |> toList
    in
        case realFirst of
            [] ->
                Nothing

            [ r ] ->
                Just ( r, rest )

            _ ->
                Nothing


splitAt_ : String -> Int -> ( String, String )
splitAt_ =
    ffi "String" "split_at"


{-| Append two strings. You can also use [the `(++)` operator](Basics#++)
to do this.

    append "butter" "fly" == "butterfly"

-}
append : String -> String -> String
append a b =
    a ++ b


{-| Concatenate many strings into one.

    concat ["never","the","less"] == "nevertheless"

-}
concat : List String -> String
concat list =
    XList.foldr (++) "" list


{-| Get the length of a string.

    length "innumerable" == 11
    length "" == 0

-}
length : String -> Int
length =
    ffi "String" "length"



{- flag nospec:+map -}


{-| Transform every character in a string

    map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"

-}
map : (Char -> Char) -> String -> String
map f str =
    str
        |> toList
        |> List.map (f >> fromChar)
        |> join ""



{- flag nospec:+filter -}


{-| Keep only the characters that satisfy the predicate.

    filter ((==) '2') "R2-D2" == "22"

-}
filter : (Char -> Bool) -> String -> String
filter f str =
    str
        |> toList
        |> List.filter f
        |> List.map fromChar
        |> join ""


{-| Reverse a string.

    reverse "stressed" == "desserts"

-}
reverse : String -> String
reverse =
    ffi "String" "reverse"



{- flag nospec:+foldl -}


{-| Reduce a string from the left.

    foldl XString.cons "" "time" == "emit"

-}
foldl : (Char -> b -> b) -> b -> String -> b
foldl f acc str =
    str
        |> toList
        |> XList.foldl f acc



{- flag nospec:+foldr -}


{-| Reduce a string from the right.

    foldr XString.cons "" "time" == "time"

-}
foldr : (Char -> b -> b) -> b -> String -> b
foldr f acc str =
    str
        |> toList
        |> XList.foldr f acc


{-| Split a string using a given separator.

    split "," "cat,dog,cow"        == ["cat","dog","cow"]
    split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]

Use [`Regex.split`](Regex#split) if you need something more flexible.

-}
split : String -> String -> List String
split pattern str =
    split_ str [ pattern ] []


type SplitOption
    = Trim Bool


split_ : String -> List String -> List SplitOption -> List String
split_ =
    ffi "String" "split"


{-| Put many strings together with a given separator.

    join "a" ["H","w","ii","n"]        == "Hawaiian"
    join " " ["cat","dog","cow"]       == "cat dog cow"
    join "/" ["home","evan","Desktop"] == "home/evan/Desktop"

-}
join : String -> List String -> String
join str list =
    join_ list str


join_ : List String -> String -> String
join_ =
    ffi "Enum" "join"


{-| Repeat a string *n* times.

    repeat 3 "ha" == "hahaha"

-}
repeat : Int -> String -> String
repeat n str =
    repeat_ str n


repeat_ : String -> Int -> String
repeat_ =
    ffi "String" "duplicate"


{-| Take a substring given a start and end index. Negative indexes
are taken starting from the *end* of the list.

    slice  7  9 "snakes on a plane!" == "on"
    slice  0  6 "snakes on a plane!" == "snakes"
    slice  0 -7 "snakes on a plane!" == "snakes on a"
    slice -6 -1 "snakes on a plane!" == "plane"

-}



-- slice 7 9 = slice 7 (9 - 7)
--                     7  2
-- slice 0 6 = slice 0 (6 - 0)
--                     0 6
-- slice 0 -7 = slice 0 (18 - 7 - 0)
--                      0  11
-- slice -6 -1 = slice ((18 - 6)) (18 - 1 - 12)
--                          12       5


slice : Int -> Int -> String -> String
slice from to str =
    let
        l =
            length str

        mirror a =
            if a < 0 then
                l + a
            else
                a

        start =
            mirror from

        len =
            (mirror to) - start
    in
        slice_ str start len


slice_ : String -> Int -> Int -> String
slice_ =
    ffi "String" "slice"


{-| Take *n* characters from the left side of a string.

    left 2 "Mulder" == "Mu"

-}
left : Int -> String -> String
left n str =
    slice 0 n str


{-| Take *n* characters from the right side of a string.

    right 2 "Scully" == "ly"

-}
right : Int -> String -> String
right n str =
    slice (negate n) (length str) str


{-| Drop *n* characters from the left side of a string.

    dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"

-}
dropLeft : Int -> String -> String
dropLeft n str =
    slice n (length str) str


{-| Drop *n* characters from the right side of a string.

    dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"

-}
dropRight : Int -> String -> String
dropRight n str =
    slice 0 (negate n) str


{-| Pad a string on both sides until it has a given length.

    pad 5 ' ' "1"   == "  1  "
    pad 5 ' ' "11"  == "  11 "
    pad 5 ' ' "121" == " 121 "

-}
pad : Int -> Char -> String -> String
pad n c str =
    let
        right =
            (length str + n) // 2

        left =
            n
    in
        str
            |> padRight right c
            |> padLeft left c


{-| Pad a string on the left until it has a given length.

    padLeft 5 '.' "1"   == "....1"
    padLeft 5 '.' "11"  == "...11"
    padLeft 5 '.' "121" == "..121"

-}
padLeft : Int -> Char -> String -> String
padLeft n c str =
    padLeading str n (fromChar c)


padLeading : String -> Int -> String -> String
padLeading =
    ffi "String" "pad_leading"


{-| Pad a string on the right until it has a given length.

    padRight 5 '.' "1"   == "1...."
    padRight 5 '.' "11"  == "11..."
    padRight 5 '.' "121" == "121.."

-}
padRight : Int -> Char -> String -> String
padRight n c str =
    padTrailing str n (fromChar c)


padTrailing : String -> Int -> String -> String
padTrailing =
    ffi "String" "pad_trailing"


{-| Get rid of whitespace on both sides of a string.

    trim "  hats  \n" == "hats"

-}
trim : String -> String
trim =
    ffi "String" "trim"


{-| Get rid of whitespace on the left of a string.

    trimLeft "  hats  \n" == "hats  \n"

-}
trimLeft : String -> String
trimLeft =
    ffi "String" "trim_leading"


{-| Get rid of whitespace on the right of a string.

    trimRight "  hats  \n" == "  hats"

-}
trimRight : String -> String
trimRight =
    ffi "String" "trim_trailing"


{-| Break a string into words, splitting on chunks of whitespace.

    words "How are \t you? \n Good?" == ["How","are","you?","Good?"]

-}
words : String -> List String
words s =
    ffi "String" "split"


{-| Break a string into lines, splitting on newlines.

    lines "How are you?\nGood?" == ["How are you?", "Good?"]

-}
lines : String -> List String
lines str =
    split "\n" str



{- No verify to make them both work for Elixir 1.6 and lower than 1.6 -}
{- flag noverify:+toUpper noverify:+toLower -}


{-| Convert a string to all upper case. Useful for case-insensitive comparisons
and VIRTUAL YELLING.

    toUpper "skinner" == "SKINNER"

-}
toUpper : String -> String
toUpper =
    ffi "String" "upcase"



-- type Encoding
--     = Default
--     | Ascii
--     | Greek


{-| Convert a string to all lower case. Useful for case-insensitive comparisons.

    toLower "X-FILES" == "x-files"

-}
toLower : String -> String
toLower =
    ffi "String" "downcase"



{- flag nospec:+any -}


{-| Determine whether *any* characters satisfy a predicate.

    any XChar.isDigit "90210" == True
    any XChar.isDigit "R2-D2" == True
    any XChar.isDigit "heart" == False

-}
any : (Char -> Bool) -> String -> Bool
any f str =
    List.any f (toList str)



{- flag nospec:+all -}


{-| Determine whether *all* characters satisfy a predicate.

    all XChar.isDigit "90210" == True
    all XChar.isDigit "R2-D2" == False
    all XChar.isDigit "heart" == False

-}
all : (Char -> Bool) -> String -> Bool
all f str =
    List.all f (toList str)


{-| See if the second string contains the first one.

    contains "the" "theory" == True
    contains "hat" "theory" == False
    contains "THE" "theory" == False

Use [`Regex.contains`](Regex#contains) if you need something more flexible.

-}
contains : String -> String -> Bool
contains pattern str =
    contains_ str pattern


contains_ : String -> String -> Bool
contains_ =
    ffi "String" " contains?"


{-| See if the second string starts with the first one.

    startsWith "the" "theory" == True
    startsWith "ory" "theory" == False

-}
startsWith : String -> String -> Bool
startsWith prefix str =
    startsWith_ str prefix


startsWith_ : String -> String -> Bool
startsWith_ prefix str =
    ffi "String" "starts_with?"


{-| See if the second string ends with the first one.

    endsWith "the" "theory" == False
    endsWith "ory" "theory" == True

-}
endsWith : String -> String -> Bool
endsWith suffix str =
    endsWith_ str suffix


endsWith_ : String -> String -> Bool
endsWith_ =
    ffi "String" "ends_with?"


{-| Get all of the indexes for a substring in another string.

    indexes "i" "Mississippi"   == [1,4,7,10]
    indexes "ss" "Mississippi"  == [2,5]
    indexes "needle" "haystack" == []

-}
indexes : String -> String -> List Int
indexes pattern str =
    matches_ str pattern
        |> List.map XTuple.first


matches_ : String -> a -> List ( Int, String )
matches_ =
    ffi ":binary" "matches"


{-| Alias for `indexes`.
-}
indices : String -> String -> List Int
indices pattern str =
    indexes pattern str


{-| Try to convert a string into an int, failing on improperly formatted strings.

    XString.toInt "123" == Ok 123
    XString.toInt "-42" == Ok -42
    XString.toInt "3.1" == Err "could not convert string '3.1' to an Int"
    XString.toInt "31a" == Err "could not convert string '31a' to an Int"

If you are extracting a number from some raw user input, you will typically
want to use [`Result.withDefault`](Result#withDefault) to handle bad data:

    XResult.withDefault 0 (XString.toInt "42") == 42
    XResult.withDefault 0 (XString.toInt "ab") == 0

-}
toInt : String -> Result String Int
toInt str =
    case toInt_ str of
        Err "argument error" ->
            Err ("could not convert string '" ++ str ++ "' to an Int")

        e ->
            e


toInt_ : String -> Result String Int
toInt_ =
    tryFfi "String" "to_integer"


{-| Try to convert a string into a float, failing on improperly formatted strings.

    XString.toFloat "123" == Ok 123.0
    XString.toFloat "-42" == Ok -42.0
    XString.toFloat "3.1" == Ok 3.1
    XString.toFloat "31a" == Err "could not convert string '31a' to a Float"

If you are extracting a number from some raw user input, you will typically
want to use [`Result.withDefault`](Result#withDefault) to handle bad data:

    XResult.withDefault 0 (XString.toFloat "42.5") == 42.5
    XResult.withDefault 0 (XString.toFloat "cats") == 0

-}
toFloat : String -> Result String Float
toFloat str =
    let
        real =
            if contains "." str then
                str
            else
                str ++ ".0"
    in
        case toFloat_ real of
            Err "argument error" ->
                Err ("could not convert string '" ++ str ++ "' to a Float")

            e ->
                e


toFloat_ : String -> Result String Float
toFloat_ =
    tryFfi "String" "to_float"


{-| Convert a string to a list of characters.

    toList "abc" == ['a','b','c']

-}
toList : String -> List Char
toList =
    ffi "String" "to_charlist"


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarily by consing, perhaps for decoding
something.

    fromList ['a','b','c'] == "abc"

-}
fromList : List Char -> String
fromList list =
    fromCharlist list
