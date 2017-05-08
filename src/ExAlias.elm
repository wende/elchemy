module ExAlias exposing (..)

import List exposing (..)
import Ast.Statement exposing (..)
import Dict exposing (Dict)
import ExContext exposing (Context, Aliases)
import Helpers exposing (..)


getAliases : Context -> List Statement -> Aliases
getAliases c list =
    list
        |> foldl (registerAlias c) Dict.empty


registerAlias : Context -> Statement -> Aliases -> Aliases
registerAlias c s ls =
    case s of
        TypeDeclaration (TypeConstructor [ name ] _) a ->
            Dict.insert name (c.mod, TypeVariable (toSnakeCase name)) ls

        TypeAliasDeclaration (TypeConstructor [ name ] _) a ->
            Dict.insert name (c.mod, a) ls

        _ ->
            ls


maybeAlias : Aliases -> String -> Maybe (String, Type)
maybeAlias aliases name =
    Dict.get name aliases
