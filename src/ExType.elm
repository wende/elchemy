module ExType exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Helpers exposing (..)
import List exposing (..)
import ExContext exposing (Context)
import ExAlias
import Dict


flattenTypeApplication : Type -> List Type
flattenTypeApplication application =
    case application of
        TypeApplication left right ->
            left :: flattenTypeApplication right

        other ->
            [ other ]


elixirTFlat =
    elixirT True


elixirTNoFlat =
    elixirT False


find : (a -> Bool) -> List a -> Maybe a
find f list =
    list
        |> foldl
            (\a acc ->
                if f a then
                    Just a
                else
                    acc
            )
            Nothing


elixirT : Bool -> Context -> Type -> String
elixirT flatten c t =
    case t of
        TypeTuple [] ->
            "no_return"

        TypeTuple [ a ] ->
            elixirT flatten c a

        TypeTuple ((a :: rest) as list) ->
            "{"
                ++ (map (elixirT flatten c) list
                        |> String.join ", "
                   )
                ++ "}"

        TypeVariable "number" ->
            "number"

        (TypeVariable name) as var ->
            c.aliases
                |> Dict.values
                |> find (\( _, a ) -> a == var)
                |> (\a ->
                        case a of
                            Just ( mod, _ ) ->
                                if mod == c.mod then
                                    name
                                else
                                    mod ++ "." ++ name

                            Nothing ->
                                "any"
                   )

        TypeConstructor [ t ] any ->
            elixirTypeConstructor flatten c t any

        TypeConstructor t [] ->
            case lastAndRest t of
                ( Just last, a ) ->
                    String.join "." a
                        ++ "."
                        ++ toSnakeCase True last

                _ ->
                    Debug.crash "Shouldn't ever happen"

        TypeRecord fields ->
            "%{"
                ++ (map
                        (\( k, v ) ->
                            k ++ ": " ++ elixirT flatten c v
                        )
                        fields
                        |> String.join ", "
                   )
                ++ "}"

        TypeApplication l r ->
            if flatten then
                "("
                    ++ (flattenTypeApplication r
                            |> lastAndRest
                            |> \( last, rest ) ->
                                (map (elixirT flatten c) (l :: rest) |> String.join ", ")
                                    ++ " -> "
                                    ++ (Maybe.map (elixirT flatten c) last |> Maybe.withDefault "")
                       )
                    ++ ")"
            else
                "("
                    ++ elixirT flatten c l
                    ++ " -> "
                    ++ elixirT flatten c r
                    ++ ")"

        other ->
            notImplemented "type" other


elixirTypeConstructor : Bool -> Context -> String -> List Type -> String
elixirTypeConstructor flatten c name args =
    case ( name, args ) of
        ( "String", [] ) ->
            "String.t"

        ( "Char", [] ) ->
            "char_list"

        ( "Bool", [] ) ->
            "boolean"

        ( "Int", [] ) ->
            "integer"

        ( "Pid", [] ) ->
            "pid"

        ( "Float", [] ) ->
            "float"

        ( "List", [ t ] ) ->
            "list(" ++ elixirT flatten c t ++ ")"

        ( "Maybe", [ t ] ) ->
            elixirT flatten c t ++ " | nil"

        ( "Nothing", [] ) ->
            "nil"

        ( "Just", [ t ] ) ->
            elixirT flatten c t

        ( "Err", [ t ] ) ->
            "{:error, " ++ elixirT flatten c t ++ "}"

        ( "Ok", [ t ] ) ->
            if t == TypeTuple [] then
                "ok"
            else
                "{:ok," ++ elixirT flatten c t ++ "}"

        ( "T", [] ) ->
            "t"

        ( t, [] ) ->
            aliasOr c t (atomize t)

        ( t, list ) ->
            aliasOr c
                t
                ("{"
                    ++ atomize t
                    ++ ", "
                    ++ (map (elixirT flatten c) list |> String.join ", ")
                    ++ "}"
                )


typespec0 : Context -> Type -> String
typespec0 c t =
    "() :: " ++ elixirTNoFlat c t


typespec : Context -> Type -> String
typespec c t =
    case lastAndRest (flattenTypeApplication t) of
        ( Just last, args ) ->
            "("
                ++ (map (elixirTNoFlat c) args
                        |> String.join ", "
                   )
                ++ ") :: "
                ++ elixirTNoFlat c last

        ( Nothing, _ ) ->
            Debug.crash "impossible"


typealias : Context -> Type -> String
typealias c t =
    case t of
        TypeApplication tc t ->
            typealias c tc ++ typealias c t

        (TypeConstructor _ _) as t ->
            elixirTNoFlat c t

        TypeVariable name ->
            name

        other ->
            notImplemented "typealias" other


typealiasConstructor : ( String, Type ) -> Expression
typealiasConstructor modAndAlias =
    case modAndAlias of
        ( _, TypeConstructor [ name ] _ ) ->
            Variable [ name ]

        ( _, TypeRecord kvs ) ->
            let
                args =
                    List.length kvs
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")

                varargs =
                    kvs
                        |> List.map2 (flip (,)) args
                        |> List.map (Tuple.mapFirst Tuple.first)
                        |> List.map
                            (Tuple.mapSecond (singleton >> Variable))
            in
                Lambda (map (singleton >> Variable) args) (Record varargs)

        ( _, TypeTuple kvs ) ->
            let
                args =
                    List.length kvs
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")
                        |> map (singleton >> Variable)
            in
                Lambda (args) (Tuple args)

        _ ->
            Debug.crash "Only simple type aliases. Sorry"


constructApplication : List String -> List Expression
constructApplication list =
    case list of
        [] ->
            Debug.crash "Wrong application"

        [ one ] ->
            [ Variable [ one ] ]

        head :: tail ->
            [ foldl (\a acc -> Application acc (Variable [ a ]))
                (Variable [ head ])
                tail
            ]


aliasOr : Context -> String -> String -> String
aliasOr c name default =
    ExAlias.maybeAlias c.aliases name
        |> Maybe.map (Tuple.second >> elixirTNoFlat c)
        |> Maybe.withDefault (default)
