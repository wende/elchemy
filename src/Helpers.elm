module Helpers exposing (..)

import Char
import Dict exposing (Dict)
import Ast.Statement exposing (Type(..))
import Ast.Expression exposing (Expression(..))
import Regex exposing (Regex(..), HowMany(..), regex)


type MaybeUpper
    = Upper String
    | Lower String


{-| Throw a nice error saying that this feature is not implemented yet
-}
notImplemented : String -> a -> String
notImplemented feature value =
    " ## ERROR: No "
        ++ feature
        ++ " implementation for "
        ++ toString value
        ++ " yet"
        ++ "\n"
        |> Debug.crash


{-| Convert string to snakecase, if the flag is set to true then it won't replace reserved words
-}
toSnakeCase : Bool -> String -> String
toSnakeCase isntAtom s =
    let
        string =
            if isntAtom then
                replaceReserved s
            else
                s
    in
        if String.toUpper string == string then
            String.toLower string
        else
            string
                |> Regex.split Regex.All (Regex.regex "(?=[A-Z])")
                |> String.join "_"
                |> String.toLower


capitalize : String -> String
capitalize s =
    String.uncons s
        |> Maybe.map (Tuple.mapFirst Char.toUpper >> uncurry String.cons)
        |> Maybe.withDefault ""


atomize : String -> String
atomize s =
    ":" ++ toSnakeCase False s


{-| Returns if string start with uppercase
-}
isCapitilzed : String -> Bool
isCapitilzed s =
    String.uncons s
        |> Maybe.map (Tuple.first >> Char.isUpper)
        |> Maybe.withDefault False


indNoNewline : Int -> String
indNoNewline i =
    List.repeat ((i + 1) * 2) " " |> String.join ""


ind : Int -> String
ind i =
    "\n" ++ indNoNewline i


prependAll : String -> String -> String
prependAll with target =
    String.lines target
        |> List.map
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
    , ( "<|", "" )
    , ( "<<", "" )
    , ( "|>", "|>" )

    -- Exception
    , ( "%", "rem" )

    -- Exception
    , ( "//", "div" )

    -- Exception
    --, ( "rem", "rem" )
    -- Exception
    , ( "^", "" )

    -- Exception
    , ( "::", "cons" )
    , ( "not", "!" )
    , ( ",", "tuple2" )
    , ( ",,", "tuple3" )
    , ( ",,,", "tuple4" )
    , ( ",,,,", "tuple5" )
    , ( "as", "=" )
    ]
        |> List.foldl (uncurry Dict.insert) Dict.empty


type Operator
    = None
    | Builtin
    | Custom


isCustomOperator : String -> Bool
isCustomOperator op =
    operatorType op == Custom


operatorType : String -> Operator
operatorType name =
    let
        is_builtin =
            operators
                |> Dict.keys
                |> List.any ((==) name)

        is_custom =
            Regex.contains (regex "^[+\\-\\/*=.$<>:&|^?%#@~!]+$") name
    in
        case ( is_builtin, is_custom ) of
            ( True, _ ) ->
                Builtin

            ( False, True ) ->
                Custom

            _ ->
                None


translateOperator : String -> String
translateOperator op =
    case Dict.get op operators of
        Just "" ->
            Debug.crash <|
                op
                    ++ " is not a valid or not implemented yet operator"

        Just key ->
            key

        _ ->
            replaceOp op


trimIndentations : String -> String
trimIndentations line =
    Regex.replace All (regex "\\s+\\n") (always "\n") line


generateArguments : Int -> List String
generateArguments =
    generateArguments_ "x"


generateArguments_ : String -> Int -> List String
generateArguments_ str n =
    List.range 1 n
        |> List.map toString
        |> List.map ((++) str)


escape : String -> String
escape s =
    Regex.replace All (regex "\\\\") (always "\\\\") s


ops : List ( Int, Char )
ops =
    [ '+', '-', '/', '*', '=', '.', '$', '<', '>', ':', '&', '|', '^', '?', '%', '#', '@', '~', '!' ] |> List.indexedMap (,)


modulePath : List String -> String
modulePath list =
    let
        snakeIfLower a =
            if isCapitilzed a then
                a
            else
                toSnakeCase True a
    in
        list
            |> List.map snakeIfLower
            |> List.map maybeReplaceStd
            |> String.join "."


maybeReplaceStd : String -> String
maybeReplaceStd s =
    if isStdModule s then
        "X" ++ s
    else
        s


isStdModule : String -> Bool
isStdModule a =
    List.member a
        [ "Basics"
        , "Bitwise"
        , "Char"
        , "Date"
        , "Debug"
        , "Dict"
        , "List"
        , "String"
        , "Maybe"
        , "Regex"
        , "Result"
        , "Set"
        , "String"
        , "Tuple"
        ]


reservedWords : List String
reservedWords =
    [ "fn", "do", "end", "cond", "receive", "or", "and" ]


replaceOp : String -> String
replaceOp op =
    String.toList op
        |> List.map replaceOp_
        |> String.join ""
        |> flip (++) "__"


replaceOp_ : Char -> String
replaceOp_ op =
    case
        List.filter (\( i, o ) -> op == o) ops
    of
        ( index, _ ) :: _ ->
            "op" ++ toString index

        _ ->
            Debug.crash "Illegal op"


replaceReserved : String -> String
replaceReserved a =
    if List.member a reservedWords then
        a ++ "__"
    else
        a


{-| Gives a String representation of module path
-}
modulePathName : List String -> String
modulePathName =
    String.join "."


{-| Change application into a list of expressions
-}
applicationToList : Expression -> List Expression
applicationToList application =
    case application of
        Application left right ->
            (applicationToList left) ++ [ right ]

        other ->
            [ other ]


{-| Change type application into a list of expressions
-}
typeApplicationToList : Type -> List Type
typeApplicationToList application =
    case application of
        TypeApplication left right ->
            left :: typeApplicationToList right

        other ->
            [ other ]


{-| Construct application, rever of applicationToList function
-}
constructApplication : List String -> List Expression
constructApplication list =
    case list of
        [] ->
            Debug.crash "Wrong application"

        [ one ] ->
            [ Variable [ one ] ]

        head :: tail ->
            [ List.foldl (\a acc -> Application acc (Variable [ a ])) (Variable [ head ]) tail ]


{-| Nicer syntax for tuples
-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixr 0 =>


{-| Take left maybe, or right maybe if Nothing
-}
maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr m1 m2 =
    case m1 of
        Just a ->
            m1

        Nothing ->
            m2


{-| Filter Maybe based on a predicate
-}
filterMaybe : (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f m =
    flip Maybe.andThen m <|
        (\a ->
            if f a then
                Just a
            else
                Nothing
        )


{-| Finds a value in a list
-}
findInList : (a -> Bool) -> List a -> Maybe a
findInList f =
    flip List.foldl Nothing <|
        (\a acc ->
            if f a then
                Just a
            else
                acc
        )
