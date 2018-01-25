module ExFfi exposing (generateFfi)

import Dict
import ExFunction
import Ast.Statement exposing (Type)
import ExVariable exposing (rememberVariables)
import Ast.Expression exposing (Expression(..))
import ExContext exposing (Context, Parser, onlyWithoutFlag)
import Helpers
    exposing
        ( applicationToList
        , generateArguments
        , generateArguments_
        , ind
        , toSnakeCase
        )


{-| Encodes and inlines a foreign function interface macro
-}
generateFfi :
    Context
    -> Parser
    -> String
    -> List (List Type)
    -> Expression
    -> String
generateFfi c elixirE name argTypes e =
    let
        typeDef =
            c.commons.modules
                |> Dict.get c.mod
                |> Maybe.andThen (.definitions >> Dict.get name)

        appList =
            applicationToList e

        uncurryArguments c =
            uncurrify c elixirE argTypes

        wrapAllInVar =
            List.map <| List.singleton >> Variable
    in
        case ( typeDef, applicationToList e ) of
            ( Nothing, _ ) ->
                Debug.crash "Ffi requires type definition"

            ( Just def, [ Variable [ "ffi" ], String mod, String fun ] ) ->
                let
                    arguments =
                        generateArguments_ "a" def.arity
                in
                    ExFunction.functionCurry c elixirE name def.arity []
                        ++ (onlyWithoutFlag c "noverify" name <|
                                ind c.indent
                                    ++ "verify as: "
                                    ++ mod
                                    ++ "."
                                    ++ fun
                                    ++ "/"
                                    ++ toString def.arity
                           )
                        ++ ind c.indent
                        ++ "def"
                        ++ ExFunction.privateOrPublic c name
                        ++ " "
                        ++ toSnakeCase True name
                        ++ "("
                        ++ (arguments |> String.join ", ")
                        ++ ")"
                        ++ ", do: "
                        ++ mod
                        ++ "."
                        ++ fun
                        ++ "("
                        ++ uncurryArguments (rememberVariables (wrapAllInVar arguments) c)
                        ++ ")"

            ( Just def, [ Variable [ "tryFfi" ], String mod, String fun ] ) ->
                let
                    arguments =
                        generateArguments_ "a" def.arity
                in
                    ExFunction.functionCurry c elixirE name def.arity []
                        ++ ind c.indent
                        ++ "def"
                        ++ ExFunction.privateOrPublic c name
                        ++ " "
                        ++ toSnakeCase True name
                        ++ "("
                        ++ (generateArguments_ "a" def.arity |> String.join ", ")
                        ++ ")"
                        ++ " do "
                        ++ ind (c.indent + 1)
                        ++ "try_catch fn -> "
                        ++ ind (c.indent + 2)
                        ++ mod
                        ++ "."
                        ++ fun
                        ++ "("
                        ++ uncurryArguments (rememberVariables (wrapAllInVar arguments) c)
                        ++ ")"
                        ++ ind (c.indent + 1)
                        ++ "end"
                        ++ ind c.indent
                        ++ "end"

            _ ->
                Debug.crash "Wrong ffi definition"


{-| Walk through function definition and uncurry all of the multi argument functions
-}
uncurrify : Context -> Parser -> List (List Type) -> String
uncurrify c elixirE argTypes =
    let
        arity =
            List.length argTypes - 1

        indexes =
            List.range 1 arity
    in
        List.map2 (,) indexes argTypes
            |> List.map
                (\( i, arg ) ->
                    case arg of
                        [] ->
                            Debug.crash "Impossible"

                        [ any ] ->
                            "a" ++ toString i

                        list ->
                            let
                                var =
                                    Variable [ "a" ++ toString i ]

                                makeFlambda =
                                    Flambda <| List.length list - 1
                            in
                                resolveFfi c elixirE (makeFlambda var)
                )
            |> String.join ", "


type Ffi
    = Lffi Expression Expression
    | Ffi Expression Expression Expression
    | TryFfi Expression Expression Expression
    | Flambda Int Expression


{-| encodes an ffi based on context and a parser
-}
resolveFfi : Context -> Parser -> Ffi -> String
resolveFfi c elixirE ffi =
    let
        combineComas args =
            (args |> List.map (elixirE c) |> String.join ",")
    in
        case ffi of
            TryFfi (String mod) (String fun) (Tuple args) ->
                "try_catch fn _ -> "
                    ++ mod
                    ++ "."
                    ++ fun
                    ++ "("
                    ++ combineComas args
                    ++ ")"
                    ++ " end"

            -- One or many arg fun
            TryFfi (String mod) (String fun) any ->
                "try_catch fn _ -> "
                    ++ mod
                    ++ "."
                    ++ fun
                    ++ "("
                    ++ elixirE c any
                    ++ ")"
                    ++ " end"

            -- Elchemy hack
            Ffi (String mod) (String fun) (Tuple args) ->
                mod ++ "." ++ fun ++ "(" ++ combineComas args ++ ")"

            -- One or many arg fun
            Ffi (String mod) (String fun) any ->
                mod ++ "." ++ fun ++ "(" ++ elixirE c any ++ ")"

            -- Elchemy hack
            Lffi (String fun) (Tuple args) ->
                fun ++ "(" ++ combineComas args ++ ")"

            -- One arg fun
            Lffi (String fun) any ->
                fun ++ "(" ++ elixirE c any ++ ")"

            Flambda arity fun ->
                let
                    args =
                        generateArguments arity
                in
                    "fn ("
                        ++ String.join "," args
                        ++ ") -> "
                        ++ elixirE c fun
                        ++ (List.map (\a -> ".(" ++ a ++ ")") args
                                |> String.join ""
                           )
                        ++ " end"

            _ ->
                Debug.crash "Wrong ffi call"
