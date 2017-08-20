module ExAlias exposing (..)

import List exposing (..)
import Ast.Statement exposing (..)
import Dict exposing (Dict)
import ExContext
    exposing
        ( Context
        , Alias
        , AliasType
        , Aliases
        , wrongArityAlias
        )


getAliases : Context -> List Statement -> Context
getAliases c list =
    foldl registerAlias c list


registerUnionType : Context -> Type -> List Type -> Context
registerUnionType c tc types =
    case tc of
        TypeConstructor [ name ] arguments ->
            c
                |> ExContext.addAlias c.mod
                    name
                    (Alias c.mod
                        (length arguments)
                        ExContext.Type
                        (TypeVariable ("@" ++ name))
                        (\_ -> (TypeVariable ("@" ++ name)))
                    )
                |> registerTypes types

        ts ->
            Debug.crash ("Wrong type declaration " ++ toString ts)


registerTypeAlias : Context -> Type -> Type -> Context
registerTypeAlias c tc t =
    case tc of
        TypeConstructor [ name ] arguments ->
            ExContext.addAlias c.mod
                name
                (Alias c.mod
                    (length arguments)
                    ExContext.TypeAlias
                    t
                    (replaceAliasArgs name arguments t)
                )
                c

        ts ->
            Debug.crash ("Wrong type alias declaration " ++ toString ts)


registerAlias : Statement -> Context -> Context
registerAlias s c =
    case s of
        TypeDeclaration tc types ->
            registerUnionType c tc types

        TypeAliasDeclaration tc t ->
            registerTypeAlias c tc t

        _ ->
            c


registerTypes : List Type -> Context -> Context
registerTypes types c =
    foldl
        (\t context ->
            case t of
                TypeConstructor [ name ] args ->
                    ExContext.addType c.mod
                        name
                        (length args)
                        context

                any ->
                    Debug.crash "Type can only start with a tag"
        )
        c
        types


replaceAliasArgs : String -> List Type -> Type -> (List Type -> Type)
replaceAliasArgs name expectedArgs return =
    (\givenArgs ->
        let
            arity =
                length givenArgs

            expected =
                length expectedArgs
        in
            if arity == expected then
                resolveTypes expectedArgs givenArgs return
            else
                wrongArityAlias expected givenArgs name
    )


resolveTypes : List Type -> List Type -> Type -> Type
resolveTypes expected given return =
    let
        expectedName n =
            case n of
                TypeVariable name ->
                    name

                other ->
                    Debug.crash
                        ("type can only take variables. "
                            ++ toString other
                            ++ "is incorrect"
                        )

        paramsWithResolution =
            map2 (,) (map expectedName expected) given
                |> foldl (uncurry Dict.insert) Dict.empty

        replace t =
            case t of
                (TypeVariable name) as default ->
                    Dict.get name paramsWithResolution
                        |> Maybe.withDefault default

                TypeConstructor names args ->
                    TypeConstructor names (map replace args)

                TypeRecordConstructor name args ->
                    TypeRecordConstructor (replace name) (map (Tuple.mapSecond replace) args)

                -- AST eror workaround
                TypeTuple [ arg ] ->
                    replace arg

                TypeTuple args ->
                    TypeTuple (map replace args)

                TypeRecord args ->
                    TypeRecord (map (Tuple.mapSecond replace) args)

                TypeApplication l r ->
                    TypeApplication (replace l) (replace r)
    in
        replace return


localAlias : String -> Context -> Maybe Alias
localAlias name context =
    ExContext.getAlias context.mod name context
