module ExAlias exposing (..)

import List exposing (..)
import Ast.Statement exposing (..)
import Dict exposing (Dict)
import ExContext exposing (Context, Alias, Aliases, AliasFunctor(..), noParamAlias, wrongArityAlias)


getAliases : Context -> List Statement -> Aliases
getAliases c list =
    list
        |> foldl (registerAlias c) Dict.empty


registerAlias : Context -> Statement -> Aliases -> Aliases
registerAlias c s ls =
    case s of
        TypeDeclaration (TypeConstructor [ name ] arguments) a ->
            Dict.insert
                name
                ( c.mod
                , TypeVariable name
                , replaceAliasArgs name arguments (TypeVariable name)
                )
                ls

        (TypeDeclaration _ _) as ts ->
            Debug.crash ("Wrong type declaration " ++ toString ts)

        TypeAliasDeclaration (TypeConstructor [ name ] arguments) a ->
            -- We need to register every type argument as an alias in the context
            Dict.insert name
                ( c.mod
                , a
                , replaceAliasArgs name arguments a
                )
                ls

        -- Hapens when replacing arguments
        TypeAliasDeclaration (TypeVariable name) a ->
            -- We need to register every type argument as an alias in the context
            Dict.insert name
                ( c.mod
                , a
                , replaceAliasArgs name [] a
                )
                ls

        (TypeAliasDeclaration _ _) as ts ->
            Debug.crash ("Wrong type alias declaration " ++ toString ts)

        _ ->
            ls


replaceAliasArgs : String -> List Type -> Type -> AliasFunctor
replaceAliasArgs name expectedArgs return =
    AliasFunctor
        (\c givenArgs ->
            let
                arity =
                    length givenArgs

                expected =
                    length expectedArgs
            in
                if arity == expected then
                    resolveTypes c expectedArgs givenArgs
                else
                    wrongArityAlias expected givenArgs name
        )


resolveTypes : Context -> List Type -> List Type -> Context
resolveTypes context expected given =
    map2 (,) expected given
        |> foldl
            (\( exp, giv ) c ->
                case exp of
                    TypeVariable name ->
                        { c
                            | aliases =
                                registerAlias c
                                    (TypeAliasDeclaration (TypeConstructor [ name ] []) giv)
                                    c.aliases
                        }

                    _ ->
                        Debug.crash "Impossible"
            )
            context


maybeAlias : Aliases -> String -> Maybe Alias
maybeAlias aliases name =
    Dict.get name aliases
