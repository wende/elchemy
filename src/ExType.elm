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
            (flattenTypeApplication left) ++ [ right ]

        other ->
            [ other ]


elirirTFlat =
    elixirT_ True


elixirT =
    elixirT_ False


elixirT_ : Bool -> Context -> Type -> String
elixirT_ flatten c t =
    case t of
        TypeTuple [] ->
            "no_return"

        TypeTuple [ a ] ->
            elixirT c a

        TypeTuple [ a, b ] ->
            "{" ++ elixirT c a ++ ", " ++ elixirT c b ++ "}"

        TypeVariable "number" ->
            "number"

        (TypeVariable name) as var ->
            c.aliases
                |> Dict.values
                |> List.member var
                |> (\a ->
                        if a then
                            name
                        else
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
            "list(" ++ elixirT c t ++ ")"

        TypeConstructor [ "Maybe" ] [ t ] ->
            elixirT c t ++ " | nil"

        TypeConstructor [ "Nothing" ] [] ->
            "nil"

        TypeConstructor [ "Just" ] [ t ] ->
            elixirT c t

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
                    ++ (map (elixirT c) list |> String.join ", ")
                    ++ "}"
                )

        TypeRecord fields ->
            "%{"
                ++ (map (\( k, v ) -> k ++ ": " ++ elixirT c v) fields
                        |> String.join ", "
                   )
                ++ "}"

        TypeApplication l r ->
            if flatten then
                "("
                    ++ (flattenTypeApplication r
                            |> lastAndRest
                            |> \( last, rest ) ->
                                (map (elixirT c) (l :: rest) |> String.join ", ")
                                    ++ " -> "
                                    ++ (Maybe.map (elixirT c) last |> Maybe.withDefault "")
                       )
                    ++ ")"
            else
                "(" ++ elixirT c l ++ " -> " ++ elixirT c r ++ ")"

        other ->
            notImplemented "type" other


typespec0 : Context -> Type -> String
typespec0 c t =
    "() :: " ++ elixirT c t


typespec : Context -> Type -> String
typespec c t =
    typespec_ True c t


typespecf : Context -> Type -> String
typespecf c t =
    typespec_ False c t


typespec_ : Bool -> Context -> Type -> String
typespec_ start c t =
    case t of
        -- Last aruments
        TypeApplication other ((TypeApplication _ _) as app) ->
            (if start then
                "("
             else
                ""
            )
                ++ typespecf c other
                ++ typespecf c app

        TypeApplication pre_last last ->
            (if start then
                "("
             else
                ""
            )
                ++ elixirT c pre_last
                ++ ") :: "
                ++ elixirT c last

        --TypeApplication tc t ->
        (TypeConstructor _ _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT c t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        (TypeVariable _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT c t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        (TypeTuple _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT c t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        (TypeRecord _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT c t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        other ->
            notImplemented "typespec" other


typealias : Context -> Type -> String
typealias c t =
    case t of
        TypeApplication tc t ->
            typealias c tc ++ typealias c t

        (TypeConstructor _ _) as t ->
            elixirT c t

        TypeVariable name ->
            name

        other ->
            notImplemented "typealias" other


aliasOr : Context -> String -> String -> String
aliasOr c name default =
    ExAlias.maybeAlias c.aliases name
        |> Maybe.map (elixirT c)
        |> Maybe.withDefault (default)
