module ExAlias exposing (..)

import List exposing (..)
import Ast.Statement exposing (..)
import Dict exposing (Dict)
import ExContext exposing (Aliases, Alias)
import Helpers exposing (..)


getAliases : List Statement -> Aliases
getAliases list =
    list
        |> foldl registerAlias Dict.empty


handleAlias : List Type -> List Type -> List Type
handleAlias args t =
    case t of
        (TypeConstructor name vars) :: rest ->
            let
                v =
                    zip args vars
                        |> List.map
                            (\( a, v ) ->
                                case a of
                                    (TypeConstructor [ name ] []) as t ->
                                        t

                                    _ ->
                                        v
                            )
            in
                (TypeConstructor name v) :: rest

        t ->
            t


resolveAlias : Bool -> List Type -> Alias
resolveAlias isAlias t =
    (\args ->
        if isAlias then
            case args of
                [] ->
                    t

                -- figure out how to map args to variables
                a ->
                    handleAlias a t
        else
            t
    )


registerAlias : Statement -> Aliases -> Aliases
registerAlias s aliases =
    case s of
        TypeDeclaration (TypeConstructor [ name ] _) a ->
            Dict.insert name
                (resolveAlias False [ (TypeVariable (toSnakeCase name)) ])
                aliases

        TypeAliasDeclaration (TypeConstructor [ name ] _) a ->
            Dict.insert name (resolveAlias True [ a ]) aliases

        _ ->
            aliases


maybeAlias : Aliases -> String -> Maybe Alias
maybeAlias aliases name =
    Dict.get name aliases
