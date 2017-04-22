module ExAlias exposing (..)

import List exposing (..)
import Ast.Statement exposing (..)
import Dict exposing (Dict)
import ExContext exposing (Aliases)
import Helpers exposing (..)

getAliases : List Statement -> Aliases
getAliases list =
    list
        |> foldl registerAlias Dict.empty

registerAlias : Statement -> Aliases -> Aliases
registerAlias s ls =
    case s of
        TypeDeclaration (TypeConstructor [name] _) a ->
            Dict.insert name (TypeVariable (toSnakeCase name)) ls
        TypeAliasDeclaration (TypeConstructor [name] _) a ->
            Dict.insert name a ls
        _ ->
            ls


maybeAlias : Aliases -> String -> Maybe Type
maybeAlias aliases name =
    Dict.get name aliases
