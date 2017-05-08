module ExType exposing (..)

import Ast.Statement exposing (..)
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
        |> foldl (\a acc -> if f a then Just a else acc) Nothing

elixirT : Bool -> Context -> Type -> String
elixirT flatten c t =
    case t of
        TypeTuple [] ->
            "no_return"

        TypeTuple [ a ] ->
            elixirT flatten c a

        TypeTuple (a :: rest as list) ->
            "{" ++ (map (elixirT flatten c) list
                        |> String.join ", ")
                ++ "}"

        TypeVariable "number" ->
            "number"

        (TypeVariable name) as var ->
            c.aliases
                |> Dict.values
                |> find (\(_, a) -> a == var)
                |> (\a ->
                        case a of
                            Just (mod, _) ->
                                if mod == c.mod then
                                    name
                                else
                                    mod ++ "." ++ name
                            Nothing ->
                                "any"
                   )

        TypeConstructor [ "String" ] [] ->
            "String.t"

        TypeConstructor [ "Bool" ] [] ->
            "boolean"

        TypeConstructor [ "Int" ] [] ->
            "integer"

        TypeConstructor [ "Pid" ] [] ->
            "pid"

        TypeConstructor [ "Float" ] [] ->
            "float"

        TypeConstructor [ "List" ] [ t ] ->
            "list(" ++ elixirT flatten c t ++ ")"

        TypeConstructor [ "Maybe" ] [ t ] ->
            elixirT flatten c t ++ " | nil"

        TypeConstructor [ "Nothing" ] [] ->
            "nil"

        TypeConstructor [ "Just" ] [ t ] ->
            elixirT flatten c t

        TypeConstructor [ "T" ] [] ->
            "t"

        TypeConstructor [ t ] [] ->
            aliasOr c t (atomize t)

        TypeConstructor t [] ->
            case lastAndRest t of
                ( Just last, a ) ->
                    String.join "." a
                        ++ "."
                        ++ toSnakeCase last

                _ ->
                    Debug.crash "Shouldn't ever happen"

        TypeConstructor [ t ] list ->
            aliasOr c
                t
                ("{"
                    ++ atomize t
                    ++ ", "
                    ++ (map (elixirT flatten c) list |> String.join ", ")
                    ++ "}"
                )

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


typespec0 : Context -> Type -> String
typespec0 c t =
    "() :: " ++ elixirTNoFlat c t


typespec : Context -> Type -> String
typespec c t =
    case lastAndRest (flattenTypeApplication t) of
        (Just last, args) ->
            "(" ++
            (map (elixirTNoFlat c) args
                |> String.join ", ")
            ++ ") :: " ++ elixirTNoFlat c last
        (Nothing, _) ->
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


aliasOr : Context -> String -> String -> String
aliasOr c name default =
    ExAlias.maybeAlias c.aliases name
        |> Maybe.map (Tuple.second >> elixirTNoFlat c)
        |> Maybe.withDefault (default)
