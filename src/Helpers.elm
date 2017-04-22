module Helpers exposing (..)

import Char
import Tuple exposing (..)
import List exposing (..)


type MaybeUpper
    = Upper String
    | Lower String


notImplemented : String -> a -> String
notImplemented feature value =
    " ## ERROR: No "
        ++ feature
        ++ " implementation for "
        ++ toString value
        ++ " yet"
        ++ "\n"
        |> Debug.crash


toSnakeCase : String -> String
toSnakeCase s =
    let
        res =
            toSnakeCase_ s
    in
        if String.startsWith "_" res && res /= "_" then
            res |> String.dropLeft 1
        else
            res


toSnakeCase_ : String -> String
toSnakeCase_ e =
    let
        withUpper =
            \a -> ( first a |> Char.isUpper, first a, second a )
    in
        case String.uncons e |> Maybe.map withUpper of
            Just ( True, a, rest ) ->
                "_" ++ String.cons (Char.toLower a) (toSnakeCase_ rest)

            Just ( False, a, rest ) ->
                String.cons a (toSnakeCase_ rest)

            Nothing ->
                ""


isUpper : String -> Bool
isUpper string =
    case String.uncons string of
        Just ( start, rest ) ->
            Char.isUpper start

        Nothing ->
            False


capitalize : String -> String
capitalize s =
    String.uncons s
        |> Maybe.map (\a -> String.cons (Char.toUpper (first a)) (second a))
        |> Maybe.withDefault ""


atomize : String -> String
atomize s =
    ":" ++ toSnakeCase s


isCapitilzed : String -> Bool
isCapitilzed s =
    String.uncons s
        |> Maybe.map (\a -> a |> first |> Char.isUpper)
        |> Maybe.withDefault False


ind : Int -> String
ind i =
    "\n" ++ (List.repeat ((i + 1) * 2) " " |> String.join "")


uncons : List a -> ( Maybe a, List a )
uncons list =
    case list of
        a :: b ->
            ( Just a, b )

        [] ->
            ( Nothing, [] )


lastAndRest : List a -> ( Maybe a, List a )
lastAndRest list =
    list
        |> List.reverse
        |> uncons
        |> Tuple.mapSecond List.reverse
