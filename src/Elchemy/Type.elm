module Elchemy.Type exposing (elixirT, getExportedTypeNames, hasReturnedType, typeAliasConstructor, typespec, uniontype)

import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Type(..))
import Dict
import Elchemy.Alias as Alias
import Elchemy.Context as Context exposing (Context, indent)
import Elchemy.Helpers as Helpers
    exposing
        ( atomize
        , filterMaybe
        , ind
        , lastAndRest
        , toSnakeCase
        , typeApplicationToList
        )


{-| Enocde any elm type
-}
elixirT : Bool -> Context -> Type -> String
elixirT flatten c t =
    case t of
        TypeTuple [] ->
            "no_return"

        TypeTuple [ a ] ->
            elixirT flatten c a

        TypeTuple ((a :: rest) as list) ->
            "{"
                ++ (List.map (elixirT flatten c) list |> String.join ", ")
                ++ "}"

        TypeVariable "number" ->
            "number"

        (TypeVariable name) as var ->
            case String.uncons name of
                Just ( '@', name ) ->
                    toSnakeCase True name

                any ->
                    if c.inTypeDefiniton then
                        name
                    else
                        "any"

        TypeConstructor t args ->
            let
                ( mod, last ) =
                    Helpers.moduleAccess c.mod t

                modulePath =
                    if mod == c.mod then
                        c.importedTypes
                            |> Dict.get last
                            |> Maybe.map (\a -> a ++ ".")
                            |> Maybe.withDefault ""
                    else
                        mod ++ "."
            in
                modulePath ++ elixirType flatten c last args

        TypeRecord fields ->
            "%{"
                ++ ind (c.indent + 1)
                ++ (fields
                        |> List.map (\( k, v ) -> toSnakeCase False k ++ ": " ++ elixirT flatten (indent c) v)
                        |> String.join ("," ++ ind (c.indent + 1))
                   )
                ++ ind c.indent
                ++ "}"

        (TypeRecordConstructor _ _) as tr ->
            "%{"
                ++ ind (c.indent + 1)
                ++ (typeRecordFields (indent c) flatten tr
                        |> String.join (", " ++ ind (c.indent + 1))
                   )
                ++ ind c.indent
                ++ "}"

        TypeApplication l r ->
            if flatten then
                typeApplicationToList r
                    |> lastAndRest
                    |> (\( last, rest ) ->
                            "("
                                ++ ((l :: rest)
                                        |> List.map (elixirT flatten (indent c))
                                        |> String.join ", "
                                   )
                                ++ " -> "
                                ++ (last
                                        |> Maybe.map (elixirT flatten c)
                                        |> Maybe.withDefault ""
                                   )
                                ++ ")"
                       )
            else
                "("
                    ++ elixirT flatten c l
                    ++ " -> "
                    ++ elixirT flatten c r
                    ++ ")"


{-| alias for elixirT with flatting of type application
-}
elixirTFlat : Context -> Type -> String
elixirTFlat =
    elixirT True


{-| alias for elixirT without flatting of type application
-}
elixirTNoFlat : Context -> Type -> String
elixirTNoFlat =
    elixirT False


{-| Return fieilds of type record as a list of string key value pairs
-}
typeRecordFields : Context -> Bool -> Type -> List String
typeRecordFields c flatten t =
    let
        keyValuePair ( k, v ) =
            k ++ ": " ++ elixirT flatten c v
    in
        case t of
            TypeRecordConstructor (TypeConstructor [ name ] args) fields ->
                let
                    inherited =
                        Context.getAlias c.mod name c
                            |> Maybe.map (\{ typeBody } -> Alias.resolveTypeBody c typeBody args)
                            |> Maybe.map (typeRecordFields c flatten)
                in
                    List.map keyValuePair fields
                        ++ Maybe.withDefault [ "" ] inherited

            TypeRecordConstructor (TypeRecord inherited) fields ->
                List.map keyValuePair <| fields ++ inherited

            TypeRecordConstructor (TypeVariable _) fields ->
                List.map keyValuePair fields

            TypeRecordConstructor (TypeTuple [ a ]) fields ->
                typeRecordFields c flatten (TypeRecordConstructor a fields)

            TypeRecordConstructor ((TypeRecordConstructor _ _) as tr) fields ->
                List.map keyValuePair fields
                    ++ typeRecordFields c flatten tr

            (TypeRecord fields) as tr ->
                List.map keyValuePair fields

            any ->
                Context.crash c ("Wrong type record constructor " ++ toString any)


{-| Translate and encode Elm type to Elixir type
-}
elixirType : Bool -> Context -> String -> List Type -> String
elixirType flatten c name args =
    case ( name, args ) of
        ( "String", [] ) ->
            "String.t"

        ( "Char", [] ) ->
            "integer"

        ( "Bool", [] ) ->
            "boolean"

        ( "Int", [] ) ->
            "integer"

        ( "Pid", [] ) ->
            "pid"

        ( "Float", [] ) ->
            "float"

        ( "List", [ t ] ) ->
            "list(" ++ elixirT flatten c t ++ ")"

        -- ( "Dict", [ key, val ] ) ->
        --     "%{}"
        ( "Maybe", [ t ] ) ->
            "{" ++ elixirT flatten c t ++ "} | nil"

        ( "Nothing", [] ) ->
            "nil"

        ( "Just", [ t ] ) ->
            elixirT flatten c t

        ( "Err", [ t ] ) ->
            "{:error, " ++ elixirT flatten c t ++ "}"

        ( "Ok", [ t ] ) ->
            if t == TypeTuple [] then
                "ok"
            else
                "{:ok," ++ elixirT flatten c t ++ "}"

        ( t, [] ) ->
            toSnakeCase True t

        -- aliasOr c t [] (atomize t)
        ( t, list ) ->
            toSnakeCase True t
                ++ "("
                ++ (List.map (elixirT flatten c) list |> String.join ", ")
                ++ ")"



-- aliasOr c t list <|
--     "{"
--         ++ atomize t
--         ++ ", "
--         ++ (List.map (elixirT flatten c) list |> String.join ", ")
--         ++ "}"


{-| Gets all types from a subset export
-}
getExportedTypeNames : Context -> String -> ExportSet -> List String
getExportedTypeNames c mod subset =
    case subset of
        SubsetExport list ->
            List.concatMap (getExportedTypeNames c mod) list

        TypeExport name _ ->
            [ name ]

        AllExport ->
            c.commons.modules
                |> Dict.get mod
                |> Maybe.map (\mod -> (mod.aliases |> Dict.keys) ++ (mod.types |> Dict.keys))
                |> Maybe.withDefault []

        FunctionExport _ ->
            []


fullImportedType : Context -> String -> String
fullImportedType c name =
    Dict.get "name" c.importedTypes
        |> Maybe.map (\a -> a ++ "." ++ name)
        |> Maybe.withDefault name


{-| Enocde a typespec with 0 arity
-}
typespec0 : Context -> Type -> String
typespec0 c t =
    "() :: " ++ elixirTNoFlat c t


{-| Encode a typespec
-}
typespec : Context -> Type -> String
typespec c t =
    case t |> typeApplicationToList |> lastAndRest of
        ( Just last, args ) ->
            "("
                ++ (List.map (elixirTNoFlat c) args
                        |> String.join ", "
                   )
                ++ ") :: "
                ++ elixirTNoFlat c last

        ( Nothing, _ ) ->
            Context.crash c "impossible"


{-| Encode a union type
-}
uniontype : Context -> Type -> String
uniontype c t =
    case t of
        TypeConstructor [ name ] [] ->
            atomize name

        TypeConstructor [ name ] list ->
            "{"
                ++ atomize name
                ++ ", "
                ++ (List.map (elixirTNoFlat c) list |> String.join ", ")
                ++ "}"

        other ->
            Context.crash c ("I am looking for union type constructor. But got " ++ toString other)


{-| Change a constructor of a type alias into an expression after resolving it from contextual alias
-}
typeAliasConstructor : List Expression -> Context.Alias -> Maybe Expression
typeAliasConstructor args ({ parentModule, aliasType, arity, body, typeBody } as ali) =
    case ( aliasType, body ) of
        ( Context.Type, _ ) ->
            Nothing

        ( _, TypeConstructor [ name ] _ ) ->
            Nothing

        ( _, TypeRecord kvs ) ->
            let
                params =
                    List.length kvs
                        |> (+) (0 - List.length args)
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")
                        |> List.map (List.singleton >> Variable)

                varargs =
                    kvs
                        |> List.map2 (flip (,)) (args ++ params)
                        |> List.map (Tuple.mapFirst Tuple.first)
            in
                Record varargs
                    |> Lambda params
                    |> Just

        -- Error in AST. Single TypeTuple are just paren app
        ( _, TypeTuple [ app ] ) ->
            typeAliasConstructor args { ali | typeBody = Context.SimpleType app }

        ( _, TypeTuple kvs ) ->
            let
                args =
                    List.length kvs
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")
                        |> List.map (List.singleton >> Variable)
            in
                Just (Lambda args (Tuple args))

        ( _, TypeVariable name ) ->
            Just (Variable [ name ])

        other ->
            Nothing


{-| Apply alias, orelse return the provided default value
-}
aliasOr : Context -> String -> List Type -> String -> String
aliasOr c name args default =
    Context.getAlias c.mod name c
        |> (Maybe.map <|
                \{ parentModule, typeBody, aliasType } ->
                    if parentModule == c.mod then
                        elixirTNoFlat c (Alias.resolveTypeBody c typeBody args)
                    else
                        case aliasType of
                            Context.Type ->
                                parentModule ++ "." ++ elixirTNoFlat c (Alias.resolveTypeBody c typeBody args)

                            Context.TypeAlias ->
                                Alias.resolveTypeBody c typeBody args
                                    |> elixirTNoFlat { c | mod = parentModule }
           )
        |> Maybe.withDefault default


hasReturnedType : Type -> Type -> Bool
hasReturnedType returned t =
    case List.reverse (typeApplicationToList t) of
        [] ->
            False

        t :: _ ->
            t == returned
