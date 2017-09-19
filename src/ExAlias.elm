module ExAlias exposing (getAliases)

import Dict exposing (Dict)
import Helpers exposing ((=>))
import Ast.Statement exposing (Statement(..), Type(..))
import ExContext
    exposing
        ( Context
        , Alias
        , AliasType
        , wrongArityAlias
        )


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
            Helpers.typeApplicationToList t |> List.length
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

                TypeConstructor names args ->
                    TypeConstructor names (List.map replace args)

                TypeRecordConstructor name args ->
                    args
                        |> List.map (Tuple.mapSecond replace)
                        |> TypeRecordConstructor (replace name)

                -- AST eror workaround
                TypeTuple [ arg ] ->
                    replace arg

                TypeTuple args ->
                    TypeTuple (List.map replace args)

                TypeRecord args ->
                    args
                        |> List.map (Tuple.mapSecond replace)
                        |> TypeRecord

                TypeApplication l r ->
                    TypeApplication (replace l) (replace r)
    in
        replace return


localAlias : String -> Context -> Maybe Alias
localAlias name context =
    ExContext.getAlias context.mod name context
