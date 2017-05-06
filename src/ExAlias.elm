module ExAlias exposing (..)

import List exposing (..)
import Ast.Statement exposing (..)
import Dict exposing (Dict)
import ExContext exposing (Aliases, Alias)


getAliases : List Statement -> Aliases
getAliases list =
    list
        |> foldl registerAlias Dict.empty


resolveAlias : List Type -> Alias
resolveAlias t =
    (\args ->
        case args of
            [] ->
                t

            a ->
                t
    )


registerAlias : Statement -> Aliases -> Aliases
registerAlias s aliases =
    case s of
        TypeDeclaration (TypeConstructor [ name ] _) a ->
            -- (TypeVariable (toSnakeCase name))
            Dict.insert name
                (resolveAlias a)
                aliases

        TypeAliasDeclaration (TypeConstructor [ name ] _) a ->
            Dict.insert name (resolveAlias [ a ]) aliases

        _ ->
            aliases


maybeAlias : Aliases -> String -> Maybe Alias
maybeAlias aliases name =
    Dict.get name aliases
