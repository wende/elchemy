module Elchemy.Meta exposing (metaDefinition)

{-| Defines a meta module for macro interactions
-}

import Ast.Expression exposing (Expression(..))
import Dict
import Elchemy.Ast as Ast
import Elchemy.Context as Context exposing (Context)
import Elchemy.Expression as Expression
import Elchemy.Helpers as Helpers exposing (ind, modulePath)


type ImportOrRequire
    = Import String String Int
    | Require String


{-| Defines the meta module for Macro usage
-}
metaDefinition : Context -> String
metaDefinition c =
    let
        defMeta meta =
            "defmodule "
                ++ c.mod
                ++ ".Meta do"
                ++ ind c.indent
                ++ requiredImports
                ++ "\n"
                ++ ind c.indent
                ++ Expression.elixirE c meta
                ++ "\nend"

        getUsedFunctions =
            Ast.walkExpressionOutwards

        addMacro t acc =
            case t of
                Variable [ name ] ->
                    c.importedFunctions
                        |> Dict.get name
                        |> Maybe.map (\( mod, arity ) -> [ Import mod name arity ])
                        |> Maybe.withDefault []
                        |> (++) acc

                Access (Variable mods) _ ->
                    Require (modulePath mods) :: acc

                _ ->
                    acc

        requiredImports =
            c.meta
                |> Maybe.map (Ast.foldExpression addMacro [])
                |> Maybe.withDefault []
                |> List.foldl insertRequirement Dict.empty
                |> Dict.toList
                |> List.map
                    (\( k, v ) ->
                        case v of
                            [] ->
                                "require " ++ k

                            other ->
                                "import "
                                    ++ k
                                    ++ ", only: ["
                                    ++ (List.map stringify other |> String.join ",")
                                    ++ "]"
                    )
                |> String.join (ind c.indent)

        stringify ( name, arity ) =
            "{:" ++ name ++ ", " ++ toString arity ++ "}"

        insertRequirement rOrI dict =
            case rOrI of
                Require mod ->
                    dict
                        |> Dict.update mod (Maybe.withDefault [] >> Just)

                Import mod name arity ->
                    dict
                        |> Dict.update mod
                            (Maybe.map ((::) ( name, arity ))
                                >> Maybe.withDefault [ ( name, arity ) ]
                                >> Just
                            )
    in
        c.meta
            |> Maybe.map defMeta
            |> Maybe.withDefault ""
