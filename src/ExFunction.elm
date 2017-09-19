module ExFunction
    exposing
        ( privateOrPublic
        , functionCurry
        , genFunctionDefinition
        , genOverloadedFunctionDefinition
        )

import Dict
import ExVariable exposing (rememberVariables)
import Ast.Expression exposing (Expression(..))
import ExContext exposing (Context, Parser, inArgs, indent)
import Helpers
    exposing
        ( operatorType
        , translateOperator
        , ind
        , Operator(..)
        , isCustomOperator
        , generateArguments
        , toSnakeCase
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
            c.modules
                |> Dict.get c.mod
                |> Maybe.andThen (.definitions >> Dict.get name)

        arity =
            typeDef |> Maybe.map .arity |> Maybe.withDefault 0
    in
        if ExContext.hasFlag "nodef" name c then
            functionCurry c elixirE name arity
        else
            functionCurry c elixirE name arity
                ++ genElixirFunc c elixirE name args (arity - List.length args) body
                ++ "\n"


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
            [ (ind c.indent)
            , "def"
            , privateOrPublic c name
            , " "
            , elixirE (c |> rememberVariables [ l ]) l
            , " "
            , translateOperator name
            , " "
            , elixirE (rememberVariables [ r ] c) r
            , " do"
            , (ind <| c.indent + 1)
            , elixirE (indent c |> rememberVariables args) body
            , ind c.indent
            , "end"
            ]
                |> String.join ""

        ( Custom, _ ) ->
            [ (ind c.indent)
            , "def"
            , privateOrPublic c name
            , " "
            , translateOperator name
            , "("
            , (args
                |> List.map (c |> rememberVariables args |> elixirE)
                |> flip (++) (generateArguments missingArgs)
                |> String.join ", "
              )
            , ") do"
            , (ind <| c.indent + 1)
            , elixirE (indent c |> rememberVariables args) body
            , (generateArguments missingArgs
                |> List.map (\a -> ".(" ++ a ++ ")")
                |> String.join ""
              )
            , ind c.indent
            , "end"
            ]
                |> String.join ""

        ( Builtin, _ ) ->
            Debug.crash
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
    if ExContext.isPrivate context name then
        "p"
    else
        ""


{-| Encodes a curry macro for the function
-}
functionCurry : Context -> Parser -> String -> Int -> String
functionCurry c elixirE name arity =
    case ( arity, ExContext.hasFlag "nocurry" name c ) of
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
            in
                [ (ind c.indent)
                , "curry"
                , privateOrPublic c name
                , " "
                , resolvedName
                , "/"
                , toString arity
                ]
                    |> String.join ""


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
            c.modules
                |> Dict.get c.mod
                |> Maybe.andThen (.definitions >> Dict.get name)

        arity =
            typeDef |> Maybe.map .arity |> Maybe.withDefault 0
    in
        if ExContext.hasFlag "nodef" name c then
            functionCurry c elixirE name arity
        else
            functionCurry c elixirE name arity
                ++ (expressions
                        |> List.map
                            (\( left, right ) ->
                                genElixirFunc c elixirE name [ left ] (arity - 1) right
                            )
                        |> List.foldr (++) ""
                        |> flip (++) "\n"
                   )
