module MyList exposing (..)
import Elmchemy exposing (..)

isEmpty : List a -> Bool
isEmpty list = list == []

length : List a -> Int
length list = ffi "Enum" "length" list

reverse : List a -> List a
reverse list = ffi "Enum" "reverse" list

member : a -> List a -> Bool
member a list = ffi "Enum" "member?" (list, a)

head : List a -> Maybe a
head a =
    case a of
        [] -> Nothing
        a :: _ -> Just a
