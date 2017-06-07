module ExType exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Helpers exposing (..)
import List exposing (..)
import ExContext exposing (Context)
import ExAlias


flattenTypeApplication : Type -> List Type
flattenTypeApplication application =
    case application of
        TypeApplication left right ->
            left :: flattenTypeApplication right

        other ->
            [ other ]


elixirTFlat : Context -> Type -> String
elixirTFlat =
    elixirT True


elixirTNoFlat : Context -> Type -> String
elixirTNoFlat =
    elixirT False


find : (a -> Bool) -> List a -> Maybe a
find f list =
    list
        |> foldl
            (\a acc ->
                if f a then
                    Just a
                else
                    acc
            )
            Nothing


elixirT : Bool -> Context -> Type -> String
elixirT flatten c t =
    case t of
        TypeTuple [] ->
            "no_return"

        TypeTuple [ a ] ->
            elixirT flatten c a

        TypeTuple ((a :: rest) as list) ->
            "{"
                ++ (map (elixirT flatten c) list
                        |> String.join ", "
                   )
                ++ "}"

        TypeVariable "number" ->
            "number"

        (TypeVariable name) as var ->
            case String.uncons name of
                Just ( '@', name ) ->
                    toSnakeCase True name

                any ->
                    "any"

        -- |> Dict.get name
        -- |> (\a ->
        --         case a of
        --             Just ( mod, (TypeVariable name) as t ) ->
        --                 if mod == c.mod then
        --                     toSnakeCase True name
        --                 else
        --                     mod ++ "." ++ name
        --
        --             Just ( mod, (TypeConstructor _ _) as t ) ->
        --                 if mod == c.mod then
        --                     elixirTNoFlat c t
        --                 else
        --                     mod ++ "." ++ name
        --
        --             Just ( mod, t ) ->
        --                 Debug.crash (toString t ++ " in module " ++ mod ++ " is too complex for current compiler")
        --
        --             Nothing ->
        --                 if isCapitilzed name then
        --                     name
        --                 else
        --                     "any"
        --    )
        TypeConstructor [ t ] any ->
            elixirTypeConstructor flatten c t any

        TypeConstructor t [] ->
            case lastAndRest t of
                ( Just last, a ) ->
                    String.join "." a
                        ++ "."
                        ++ toSnakeCase True last

                _ ->
                    Debug.crash "Shouldn't ever happen"

        TypeRecord fields ->
            "%{"
                ++ (map
                        (\( k, v ) ->
                            k ++ ": " ++ elixirT flatten c v
                        )
                        fields
                        |> String.join ", "
                   )
                ++ "}"

        (TypeRecordConstructor _ _) as tr ->
            "%{"
                ++ ((typeRecordFields c flatten tr)
                        |> String.join ", "
                   )
                ++ "}"

        TypeApplication l r ->
            if flatten then
                "("
                    ++ (flattenTypeApplication r
                            |> lastAndRest
                            |> \( last, rest ) ->
                                (map (elixirT flatten c) (l :: rest) |> String.join ", ")
                                    ++ " -> "
                                    ++ (Maybe.map (elixirT flatten c) last |> Maybe.withDefault "")
                       )
                    ++ ")"
            else
                "("
                    ++ elixirT flatten c l
                    ++ " -> "
                    ++ elixirT flatten c r
                    ++ ")"

        other ->
            notImplemented "type" other


typeRecordFields : Context -> Bool -> Type -> List String
typeRecordFields c flatten t =
    case t of
        TypeRecordConstructor (TypeConstructor [ name ] args) fields ->
            let
                inherited =
                    ExAlias.maybeAlias c.aliases name
                        |> Maybe.map (\{ getTypeBody } -> getTypeBody args)
                        |> Maybe.map (typeRecordFields c flatten)
            in
                (map
                    (\( k, v ) ->
                        k ++ ": " ++ elixirT flatten c v
                    )
                    (fields)
                )
                    ++ (Maybe.withDefault [ "KURWA" ] inherited)

        TypeRecordConstructor (TypeRecord inherited) fields ->
            (map
                (\( k, v ) ->
                    k ++ ": " ++ elixirT flatten c v
                )
                (fields ++ inherited)
            )

        TypeRecordConstructor ((TypeRecordConstructor _ _) as tr) fields ->
            (map
                (\( k, v ) ->
                    k ++ ": " ++ elixirT flatten c v
                )
                (fields)
            )
                ++ typeRecordFields c flatten tr

        any ->
            Debug.crash ("Wrong type record constructor " ++ toString any)


elixirTypeConstructor : Bool -> Context -> String -> List Type -> String
elixirTypeConstructor flatten c name args =
    case ( name, args ) of
        ( "String", [] ) ->
            "String.t"

        ( "Char", [] ) ->
            "char_list"

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
            aliasOr c t [] (atomize t)

        ( t, list ) ->
            aliasOr
                c
                t
                list
                ("{"
                    ++ atomize t
                    ++ ", "
                    ++ (map (elixirT flatten c) list |> String.join ", ")
                    ++ "}"
                )


typespec0 : Context -> Type -> String
typespec0 c t =
    "() :: " ++ elixirTNoFlat c t


typespec : Context -> Type -> String
typespec c t =
    case lastAndRest (flattenTypeApplication t) of
        ( Just last, args ) ->
            "("
                ++ (map (elixirTNoFlat c) args
                        |> String.join ", "
                   )
                ++ ") :: "
                ++ elixirTNoFlat c last

        ( Nothing, _ ) ->
            Debug.crash "impossible"


uniontype : Context -> Type -> String
uniontype c t =
    case t of
        TypeConstructor [ name ] [] ->
            atomize name

        TypeConstructor [ name ] list ->
            "{"
                ++ atomize name
                ++ ", "
                ++ (map (elixirTNoFlat c) list |> String.join ", ")
                ++ "}"

        other ->
            Debug.crash ("I am looking for union type constructor. But got " ++ toString other)


typealiasConstructor : List a -> ExContext.Alias -> Expression
typealiasConstructor args ({ mod, arity, body, getTypeBody } as ali) =
    case body of
        TypeConstructor [ name ] _ ->
            Variable [ name ]

        TypeRecord kvs ->
            let
                args =
                    List.length kvs
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")

                varargs =
                    kvs
                        |> List.map2 (flip (,)) args
                        |> List.map (Tuple.mapFirst Tuple.first)
                        |> List.map
                            (Tuple.mapSecond (singleton >> Variable))
            in
                Lambda (map (singleton >> Variable) args) (Record varargs)

        -- Error in AST. Single TypeTuple are just paren app
        TypeTuple [ app ] ->
            typealiasConstructor args { ali | getTypeBody = (\_ -> app) }

        TypeTuple kvs ->
            let
                args =
                    List.length kvs
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")
                        |> map (singleton >> Variable)
            in
                Lambda (args) (Tuple args)

        TypeVariable name ->
            Variable [ name ]

        other ->
            Debug.crash
                ("Only simple type aliases.\n"
                    ++ toString other
                    ++ " is to complex. Sorry"
                )


constructApplication : List String -> List Expression
constructApplication list =
    case list of
        [] ->
            Debug.crash "Wrong application"

        [ one ] ->
            [ Variable [ one ] ]

        head :: tail ->
            [ foldl (\a acc -> Application acc (Variable [ a ]))
                (Variable [ head ])
                tail
            ]


aliasOr : Context -> String -> List Type -> String -> String
aliasOr c name args default =
    ExAlias.maybeAlias c.aliases name
        |> Maybe.map
            (\{ mod, getTypeBody } ->
                if mod == c.mod then
                    elixirTNoFlat c (getTypeBody args)
                else
                    mod ++ "." ++ elixirTNoFlat c (getTypeBody args)
            )
        |> Maybe.withDefault default
