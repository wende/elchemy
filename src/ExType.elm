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


elixirTFlat =
    elixirT True


elixirTNoFlat =
    elixirT False


elixirT : Bool -> Context -> Type -> String
elixirT flatten c t =
    case t of
        TypeTuple [] ->
            "no_return"

        TypeTuple [ a ] ->
            elixirT flatten c a

        TypeTuple [ a, b ] ->
            "{"
                ++ elixirT flatten c a
                ++ ", "
                ++ elixirT flatten c b
                ++ "}"

        TypeVariable "number" ->
            "number"

        (TypeVariable name) as var ->
            c.aliases
                |> Dict.values
                |> List.map (\a -> a [])
                |> List.member [ var ]
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
            aliasOr c t [] (atomize t)

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
                list
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
                ++ elixirTFlat c pre_last
                ++ ") :: "
                ++ elixirTFlat c last

        --TypeApplication tc t ->
        (TypeConstructor name args) as tc ->
            let
                n =
                    Maybe.withDefault "" (List.head name)

                t =
                    case ExAlias.maybeAlias c.aliases n of
                        Just a ->
                            a args
                                |> List.map (typealias c)
                                |> String.join " | "

                        Nothing ->
                            ""
            in
                (if start then
                    " :: "
                 else
                    ""
                )
                    ++ t

        (TypeVariable _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirTFlat c t
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
                ++ elixirTFlat c t
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
                ++ elixirTFlat c t
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
            elixirTNoFlat c t

        TypeVariable name ->
            name

        -- TODO: TypeTuple
        other ->
            notImplemented "typealias" other


aliasOr : Context -> String -> List Type -> String -> String
aliasOr c name args default =
    ExAlias.maybeAlias c.aliases name
        |> Maybe.map (\a -> a args)
        |> Maybe.map
            (\a ->
                case List.head a of
                    Just a ->
                        elixirTNoFlat c a

                    Nothing ->
                        ""
            )
        |> Maybe.withDefault (default)
