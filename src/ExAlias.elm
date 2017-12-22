module ExAlias exposing (getAliases, replaceTypeAliases)

import Dict exposing (Dict)
import Helpers exposing ((=>), lastAndRest)
import Ast.Statement exposing (Statement(..), Type(..))
import ExContext
    exposing
        ( Context
        , Alias
        , AliasType
        , wrongArityAlias
        )
import ExAst


getAliases : Context -> List Statement -> Context
getAliases c list =
    List.foldl registerAlias c list


registerAlias : Statement -> Context -> Context
registerAlias s c =
    case s of
        TypeAliasDeclaration tc t ->
            registerTypeAlias c tc t

        TypeDeclaration tc types ->
            registerUnionType c tc types

        FunctionTypeDeclaration name t ->
            registerFunctionDefinition c name t

        _ ->
            c


registerTypeAlias : Context -> Type -> Type -> Context
registerTypeAlias c tc t =
    case tc of
        TypeConstructor [ name ] arguments ->
            let
                arity =
                    List.length arguments

                typeBody =
                    replaceAliasArgs name arguments t

                ali =
                    Alias c.mod arity ExContext.TypeAlias t typeBody []
            in
                ExContext.addAlias c.mod name ali c

        ts ->
            Debug.crash <| "Wrong type alias declaration " ++ toString ts


registerUnionType : Context -> Type -> List Type -> Context
registerUnionType c tc types =
    case tc of
        TypeConstructor [ name ] arguments ->
            let
                typeVar =
                    TypeVariable <| "@" ++ name

                typeBody =
                    always typeVar

                arity =
                    List.length arguments

                ( names, newC ) =
                    registerTypes types name c

                ali =
                    Alias c.mod arity ExContext.Type typeVar typeBody names
            in
                ExContext.addAlias c.mod name ali newC

        ts ->
            Debug.crash <| "Wrong type declaration " ++ toString ts


registerFunctionDefinition : Context -> String -> Type -> Context
registerFunctionDefinition c name t =
    let
        arity =
            replaceTypeAliases c t
                |> Helpers.typeApplicationToList
                |> List.length
    in
        ExContext.addDefinition c name (ExContext.Definition (arity - 1) t)


registerTypes : List Type -> String -> Context -> ( List String, Context )
registerTypes types parentAlias c =
    let
        addType t ( names, context ) =
            case t of
                TypeConstructor [ name ] args ->
                    (name :: names)
                        => ExContext.addType c.mod parentAlias name (List.length args) context

                any ->
                    Debug.crash "Type can only start with a tag"
    in
        List.foldl addType ( [], c ) types


replaceAliasArgs : String -> List Type -> Type -> (List Type -> Type)
replaceAliasArgs name expectedArgs return =
    (\givenArgs ->
        let
            arity =
                List.length givenArgs

            expected =
                List.length expectedArgs
        in
            if arity == expected then
                resolveTypes expectedArgs givenArgs return
            else
                wrongArityAlias expected givenArgs name
    )


{-| Function taking a type and replacing all aliases it points to with their dealiased version
-}
replaceTypeAliases : Context -> Type -> Type
replaceTypeAliases c t =
    let
        mapOrFunUpdate mod default typeName args =
            ExContext.getAlias mod typeName c
                |> Helpers.filterMaybe (.aliasType >> (==) ExContext.TypeAlias)
                |> Maybe.map (\{ getTypeBody } -> getTypeBody args)
                |> Maybe.andThen
                    (\body ->
                        case body of
                            TypeRecordConstructor _ _ ->
                                Just body

                            TypeApplication _ _ ->
                                Just body

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault default

        typeConstructorReplace default fullType args =
            Helpers.moduleAccess c.mod fullType
                |> \( mod, typeName ) -> mapOrFunUpdate mod default typeName args

        replaceAlias t =
            case t of
                TypeConstructor fullType args ->
                    typeConstructorReplace t fullType args

                t ->
                    t
    in
        ExAst.walkTypeOutwards replaceAlias t


resolveTypes : List Type -> List Type -> Type -> Type
resolveTypes expected given return =
    let
        expectedName n =
            case n of
                TypeVariable name ->
                    name

                other ->
                    Debug.crash <|
                        "type can only take variables. "
                            ++ toString other
                            ++ "is incorrect"

        paramsWithResolution =
            List.map2 (,) (List.map expectedName expected) given
                |> List.foldl (uncurry Dict.insert) Dict.empty

        replace t =
            case t of
                (TypeVariable name) as default ->
                    Dict.get name paramsWithResolution
                        |> Maybe.withDefault default

                t ->
                    t
    in
        ExAst.walkTypeOutwards replace return


localAlias : String -> Context -> Maybe Alias
localAlias name context =
    ExContext.getAlias context.mod name context
