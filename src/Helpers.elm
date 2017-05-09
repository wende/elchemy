module Helpers exposing (..)

import Char
import Tuple exposing (..)
import List exposing (..)
import ExContext exposing (Context)
import Regex exposing (..)
import Dict exposing (Dict)


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
toSnakeCase string =
    if String.toUpper string == string then
        String.toLower string
    else
        string
            |> Regex.split Regex.All (Regex.regex "(?=[A-Z])")
            |> String.join "_"
            |> String.toLower


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


prependAll : String -> String -> String
prependAll with target =
    String.lines target
        |> map
            (\line ->
                if String.trim line == "" then
                    line
                else
                    with ++ line
            )
        |> String.join "\n"


indAll : Int -> String -> String
indAll i s =
    "\n" ++ prependAll (String.dropLeft 1 (ind i)) s


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


unquoteSplicing : String -> String
unquoteSplicing =
    Regex.replace All (regex "(^\\{|\\}$)") (\_ -> "")


operators : Dict String String
operators =
    [ ( "||", "||" )
    , ( "&&", "&&" )
    , ( "==", "==" )
    , ( "/=", "!=" )
    , ( "<", "<" )
    , ( ">", ">" )
    , ( ">=", ">=" )
    , ( "<=", "<=" )
    , ( "++", "++" )
    , ( "+", "+" )
    , ( "-", "-" )
    , ( "*", "*" )
    , ( "/", "/" )
    , ( ">>", ">>>" )
    , ( "<|", "<<<" )

    -- Exception
    , ( "%", "rem" )

    -- Exception
    , ( "//", "" )

    -- Exception
    , ( "rem", "" )

    -- Exception
    , ( "^", "" )
    , ( "<<", "" )
    , ( "|>", "|>" )

    -- Exception
    , ( "::", "cons" )
    , ( "not", "!" )
    , ( ",", "tuple2" )
    , ( ",,", "tuple3" )
    , ( ",,,", "tuple4" )
    , ( ",,,,", "tuple5" )
    ]
        |> List.foldl (uncurry Dict.insert) Dict.empty


isOperator : String -> Bool
isOperator name =
    operators
        |> Dict.keys
        |> List.any ((==) name)


translateOperator : String -> String
translateOperator op =
    case Dict.get op operators of
        Just "" ->
            Debug.crash
                (op
                    ++ " is not a valid or not implemented yet operator"
                )

        Just key ->
            key

        _ ->
            Debug.crash
                (op
                    ++ " is not a valid or not implemented yet operator"
                )


trimIndentations : String -> String
trimIndentations line =
    Regex.replace All (regex "\\s+\\n") (always "\n") line

generateArguments : Int -> List String
generateArguments n =
    List.range 1 n
        |> map toString
        |> map ((++) "x")


unescape : String -> String
unescape s =
    Regex.replace All (regex "\\\\\\\\") (always "\\") s

escape : String -> String
escape s =
    Regex.replace All (regex "\\\\n") (always "\\\\n") s
