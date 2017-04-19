module ExType exposing (..)

import Ast.Statement exposing (..)
import Helpers exposing (..)
import List exposing (..)


flattenTypeApplication : Type -> List Type
flattenTypeApplication application =
    case application of
        TypeApplication left right ->
            (flattenTypeApplication left) ++ [ right ]

        other ->
            [ other ]


elixirT : Type -> String
elixirT t =
    case t of
        TypeTuple [ a ] ->
            elixirT a

        TypeTuple [ a, b ] ->
            "{" ++ elixirT a ++ ", " ++ elixirT b ++ "}"

        TypeVariable "number" ->
            "number"

        TypeVariable name ->
            "any"

        TypeConstructor [ "String" ] [] ->
            "String.t"

        TypeConstructor [ "Int" ] [] ->
            "int"

        TypeConstructor [ "List" ] [ t ] ->
            "list(" ++ elixirT t ++ ")"

        TypeConstructor [ "Maybe" ] [ t ] ->
            elixirT t ++ " | nil"

        TypeConstructor [ "T" ] [] ->
            "t"

        TypeConstructor [ t ] [] ->
            toSnakeCase t

        TypeConstructor t [] ->
            -- (String.join "." t) ++ ".t"
            case lastAndRest t of
                ( Just last, a ) ->
                    last

                _ ->
                    Debug.crash "Shouldn't ever happen"

        TypeConstructor [ t ] list ->
            "{"
                ++ atomize t
                ++ ", "
                ++ (map elixirT list |> String.join ", ")
                ++ "}"

        TypeRecord fields ->
            "%{"
                ++ (map (\( k, v ) -> k ++ ": " ++ elixirT v) fields
                        |> String.join ", "
                   )
                ++ "}"

        TypeApplication l r ->
            "("
                ++ (flattenTypeApplication r
                        |> lastAndRest
                        |> \( last, rest ) ->
                            (map elixirT (l :: rest) |> String.join ", ")
                                ++ " -> "
                                ++ (Maybe.map elixirT last |> Maybe.withDefault "")
                   )
                ++ ")"

        other ->
            notImplemented "type" other


typespec : Type -> String
typespec t =
    typespec_ True t


typespecf : Type -> String
typespecf t =
    typespec_ False t


typespec_ : Bool -> Type -> String
typespec_ start t =
    case t of
        -- Last aruments
        TypeApplication other ((TypeApplication _ _) as app) ->
            (if start then
                "("
             else
                ""
            )
                ++ typespecf other
                ++ typespecf app

        TypeApplication pre_last last ->
            (if start then
                "("
             else
                ""
            )
                ++ elixirT pre_last
                ++ ") :: "
                ++ elixirT last

        --TypeApplication tc t ->
        (TypeConstructor _ _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT t
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
                ++ elixirT t
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
                ++ elixirT t
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
                ++ elixirT t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        other ->
            notImplemented "typespec" other


typealias : Type -> String
typealias t =
    case t of
        TypeApplication tc t ->
            typealias tc ++ typealias t

        (TypeConstructor _ _) as t ->
            elixirT t

        TypeVariable name ->
            name

        other ->
            notImplemented "typealias" other
