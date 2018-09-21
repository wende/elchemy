module Elchemy.Function
    exposing
        ( functionCurry
        , genFunctionDefinition
        , genOverloadedFunctionDefinition
        , privateOrPublic
        )

import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (Type)
import Dict
import Elchemy.Context as Context exposing (Context, Parser, inArgs, indent)
import Elchemy.Variable as Variable exposing (rememberVariables)
import Elchemy.Helpers as Helpers
    exposing
        ( Operator(..)
        , generateArguments
        , ind
        , isCustomOperator
        , operatorType
        , toSnakeCase
        , translateOperator
        )


{-| Encodes a function defintion with all decorations like curry and type spec
-}
genFunctionDefinition :
    Context
    -> Parser
    -> String
    -> List Expression
    -> Expression
    -> String
genFunctionDefinition c elixirE name args body =
    let
        typeDef =
            c.commons.modules
                |> Dict.get c.mod
                |> Maybe.andThen (.definitions >> Dict.get name)

        arity =
            typeDef |> Maybe.map .arity |> Maybe.withDefault 0

        lambdasAt =
            getLambdaArgumentIndexes (Maybe.map .def typeDef)
    in
        if Context.hasFlag "nodef" name c then
            functionCurry c elixirE name arity lambdasAt
        else
            functionCurry c elixirE name arity lambdasAt
                ++ genElixirFunc c elixirE name args (arity - List.length args) body
                ++ "\n"


{-| Generates an overloaded function defintion when body is matched on case
-}
genOverloadedFunctionDefinition :
    Context
    -> Parser
    -> String
    -> List Expression
    -> Expression
    -> List ( Expression, Expression )
    -> String
genOverloadedFunctionDefinition c elixirE name args body expressions =
    let
        typeDef =
            c.commons.modules
                |> Dict.get c.mod
                |> Maybe.andThen (.definitions >> Dict.get name)

        arity =
            typeDef |> Maybe.map .arity |> Maybe.withDefault 0

        lambdasAt =
            getLambdaArgumentIndexes (Maybe.map .def typeDef)

        pairAsArgs asArgs =
            asArgs
                |> List.map2 (flip <| BinOp <| Variable [ "as" ]) args

        caseBranch ( left, right ) =
            case left of
                Tuple matchedArgs ->
                    genElixirFunc c elixirE name (pairAsArgs matchedArgs) (arity - List.length (pairAsArgs matchedArgs)) right

                _ ->
                    genElixirFunc c elixirE name (pairAsArgs [ left ]) (arity - 1) right
    in
        if Context.hasFlag "nodef" name c then
            functionCurry c elixirE name arity lambdasAt
        else
            functionCurry c elixirE name arity lambdasAt
                ++ (expressions
                        |> List.map caseBranch
                        |> List.foldr (++) ""
                        |> flip (++) "\n"
                   )


{-| Encodes a function defintion based on given params
-}
genElixirFunc :
    Context
    -> Parser
    -> String
    -> List Expression
    -> Int
    -> Expression
    -> String
genElixirFunc c elixirE name args missingArgs body =
    case ( operatorType name, args ) of
        ( Builtin, [ l, r ] ) ->
            [ ind c.indent
            , "def"
            , privateOrPublic c name
            , " "
            , elixirE (c |> rememberVariables [ l ]) l
            , " "
            , translateOperator name
            , " "
            , elixirE (rememberVariables [ r ] c) r
            , " do"
            , ind <| c.indent + 1
            , elixirE (indent c |> rememberVariables args) body
            , ind c.indent
            , "end"
            ]
                |> String.join ""

        ( Custom, _ ) ->
            [ ind c.indent
            , "def"
            , privateOrPublic c name
            , " "
            , translateOperator name
            , "("
            , args
                |> List.map (c |> rememberVariables args |> elixirE)
                |> flip (++) (generateArguments missingArgs)
                |> String.join ", "
            , ") do"
            , ind <| c.indent + 1
            , elixirE (indent c |> rememberVariables args) body
            , generateArguments missingArgs
                |> List.map (\a -> ".(" ++ a ++ ")")
                |> String.join ""
            , ind c.indent
            , "end"
            ]
                |> String.join ""

        ( Builtin, _ ) ->
            Context.crash c
                ("operator " ++ name ++ " has to have 2 arguments but has " ++ toString args)

        ( None, _ ) ->
            let
                missing =
                    generateArguments missingArgs

                wrapIfMiss s =
                    if List.length missing > 0 then
                        s
                    else
                        ""

                missingVarargs =
                    List.map (List.singleton >> Variable) missing
            in
                [ ind c.indent
                , "def"
                , privateOrPublic c name
                , " "
                , toSnakeCase True name
                , "("
                , args
                    ++ missingVarargs
                    |> List.map (c |> inArgs |> elixirE)
                    |> String.join ", "
                , ") do"
                , ind <| c.indent + 1
                , wrapIfMiss "("
                , elixirE (indent c |> rememberVariables (args ++ missingVarargs)) body
                , wrapIfMiss ")"
                , missing
                    |> List.map (\a -> ".(" ++ a ++ ")")
                    |> String.join ""
                , ind c.indent
                , "end"
                ]
                    |> String.join ""


{-| Returns "p" if a function is private. Else returns empty string
-}
privateOrPublic : Context -> String -> String
privateOrPublic context name =
    if Context.isPrivate context name then
        "p"
    else
        ""


{-| Encodes a curry macro for the function
-}
functionCurry : Context -> Parser -> String -> Int -> List ( Int, Int ) -> String
functionCurry c elixirE name arity lambdasAt =
    case ( arity, Context.hasFlag "nocurry" name c ) of
        ( 0, _ ) ->
            ""

        ( _, True ) ->
            ""

        ( arity, False ) ->
            let
                resolvedName =
                    if isCustomOperator name then
                        translateOperator name
                    else
                        toSnakeCase True name

                p =
                    privateOrPublic c name

                lambdas =
                    lambdasAt
                        |> List.map (\( a, b ) -> "{" ++ toString a ++ ", " ++ toString b ++ "}")
            in
                if lambdas == [] || p == "p" then
                    [ ind c.indent
                    , "curry"
                    , " "
                    , resolvedName
                    , "/"
                    , toString arity
                    ]
                        |> String.join ""
                else
                    [ ind c.indent
                    , "curry"
                    , " "
                    , resolvedName
                    , "/"
                    , toString arity
                    , ", lambdas: ["
                    , lambdas |> String.join ", "
                    , "]"
                    ]
                        |> String.join ""


{-| Gives indexes (starting from 0) of the arguments which
are lambdas with arity bigger than 1
-}
getLambdaArgumentIndexes : Maybe Type -> List ( Int, Int )
getLambdaArgumentIndexes t =
    Maybe.map Helpers.typeApplicationToList t
        |> Maybe.withDefault []
        |> List.map Helpers.typeApplicationToList
        |> List.indexedMap (,)
        -- -1 since a -> b is not 2 arity
        |> List.map (Tuple.mapSecond <| List.length >> (+) -1)
        |> List.filter (\( _, r ) -> r > 1)
