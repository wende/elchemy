module ExFfi exposing (..)

import Ast.Statement exposing (Type)
import Ast.Expression exposing (..)
import ExContext exposing (Context, Parser, onlyWithoutFlag)
import Dict
import Helpers exposing (..)
import ExFunction exposing (..)
import ExVariable exposing (rememberVariables)


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
            c.definitions |> Dict.get name

        appList =
            applicationToList e

        flambdaArguments c =
            flambdify c elixirE argTypes
    in
        case ( typeDef, applicationToList e ) of
            ( Nothing, _ ) ->
                Debug.crash "Ffi requires type definition"

            ( Just def, [ Variable [ "ffi" ], String mod, String fun ] ) ->
                let
                    arguments =
                        generateArguments_ "a" def.arity
                in
                    functionCurry c elixirE name def.arity
                        ++ (onlyWithoutFlag c
                                "noverify"
                                name
                                (ind c.indent
                                    ++ "verify as: "
                                    ++ mod
                                    ++ "."
                                    ++ fun
                                    ++ "/"
                                    ++ toString def.arity
                                )
                           )
                        ++ ind c.indent
                        ++ "def"
                        ++ privateOrPublic c name
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
                        ++ flambdaArguments
                            (rememberVariables
                                (List.map (List.singleton >> Variable) arguments)
                                c
                            )
                        ++ ")"

            ( Just def, [ Variable [ "tryFfi" ], String mod, String fun ] ) ->
                let
                    arguments =
                        generateArguments_ "a" def.arity
                in
                    functionCurry c elixirE name def.arity
                        ++ ind c.indent
                        ++ "def"
                        ++ privateOrPublic c name
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
                        ++ flambdaArguments
                            (rememberVariables
                                (List.map (List.singleton >> Variable) arguments)
                                c
                            )
                        ++ ")"
                        ++ ind (c.indent + 1)
                        ++ "end"
                        ++ ind c.indent
                        ++ "end"

            _ ->
                Debug.crash "Wrong ffi definition"


flambdify : Context -> Parser -> List (List Type) -> String
flambdify c elixirE argTypes =
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
                            resolveFfi c
                                (Flambda
                                    (List.length list - 1)
                                    (Variable [ "a" ++ toString i ])
                                )
                                elixirE
                )
            |> String.join ", "


type Ffi
    = Lffi Expression Expression
    | Ffi Expression Expression Expression
    | TryFfi Expression Expression Expression
    | Flambda Int Expression


resolveFfi : Context -> Ffi -> Parser -> String
resolveFfi c ffi elixirE =
    case ffi of
        TryFfi (String mod) (String fun) ((Tuple _) as args) ->
            "try_catch fn _ -> "
                ++ mod
                ++ "."
                ++ fun
                ++ "("
                ++ combineComas c elixirE args
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
        Ffi (String mod) (String fun) ((Tuple _) as args) ->
            mod ++ "." ++ fun ++ "(" ++ combineComas c elixirE args ++ ")"

        -- One or many arg fun
        Ffi (String mod) (String fun) any ->
            mod ++ "." ++ fun ++ "(" ++ elixirE c any ++ ")"

        -- Elchemy hack
        Lffi (String fun) ((Tuple _) as args) ->
            fun ++ "(" ++ combineComas c elixirE args ++ ")"

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
