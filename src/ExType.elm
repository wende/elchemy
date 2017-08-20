module ExType exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Helpers exposing (..)
import List exposing (..)
import ExContext exposing (Context, indent)


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

        TypeConstructor [ t ] any ->
            elixirTypeConstructor flatten c t any

        TypeConstructor t args ->
            case lastAndRest t of
                ( Just last, a ) ->
                    ExContext.getAlias c.mod last c
                        |> Maybe.andThen
                            (\ali ->
                                if ali.aliasType == ExContext.TypeAlias then
                                    Just ali
                                else
                                    Nothing
                            )
                        |> Maybe.map (\{ getTypeBody } -> getTypeBody args)
                        |> Maybe.map (elixirT flatten c)
                        |> Maybe.withDefault
                            (String.join
                                "."
                                a
                                ++ "."
                                ++ toSnakeCase True last
                            )

                _ ->
                    Debug.crash "Shouldn't ever happen"

        TypeRecord fields ->
            "%{"
                ++ ind (c.indent + 1)
                ++ (map
                        (\( k, v ) ->
                            k ++ ": " ++ elixirT flatten (indent c) v
                        )
                        fields
                        |> String.join ("," ++ ind (c.indent + 1))
                   )
                ++ ind (c.indent)
                ++ "}"

        (TypeRecordConstructor _ _) as tr ->
            "%{"
                ++ ind (c.indent + 1)
                ++ ((typeRecordFields (indent c) flatten tr)
                        |> String.join (", " ++ ind (c.indent + 1))
                   )
                ++ ind (c.indent)
                ++ "}"

        TypeApplication l r ->
            if flatten then
                "("
                    ++ (flattenTypeApplication r
                            |> lastAndRest
                            |> \( last, rest ) ->
                                (map (elixirT flatten (indent c)) (l :: rest) |> String.join ", ")
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


typeRecordFields : Context -> Bool -> Type -> List String
typeRecordFields c flatten t =
    case t of
        TypeRecordConstructor (TypeConstructor [ name ] args) fields ->
            let
                inherited =
                    ExContext.getAlias c.mod name c
                        |> Maybe.map (\{ getTypeBody } -> getTypeBody args)
                        |> Maybe.map (typeRecordFields c flatten)
            in
                (map
                    (\( k, v ) ->
                        k ++ ": " ++ elixirT flatten c v
                    )
                    (fields)
                )
                    ++ (Maybe.withDefault [ "" ] inherited)

        TypeRecordConstructor (TypeRecord inherited) fields ->
            (map
                (\( k, v ) ->
                    k ++ ": " ++ elixirT flatten c v
                )
                (fields ++ inherited)
            )

        TypeRecordConstructor (TypeVariable _) fields ->
            (map
                (\( k, v ) ->
                    k ++ ": " ++ elixirT flatten c v
                )
                (fields)
            )

        TypeRecordConstructor (TypeTuple [ a ]) fields ->
            typeRecordFields c flatten (TypeRecordConstructor a fields)

        TypeRecordConstructor ((TypeRecordConstructor _ _) as tr) fields ->
            (map
                (\( k, v ) ->
                    k ++ ": " ++ elixirT flatten c v
                )
                (fields)
            )
                ++ typeRecordFields c flatten tr

        (TypeRecord fields) as tr ->
            (map
                (\( k, v ) ->
                    k ++ ": " ++ elixirT flatten c v
                )
                (fields)
            )

        any ->
            Debug.crash ("Wrong type record constructor " ++ toString any)


elixirTypeConstructor : Bool -> Context -> String -> List Type -> String
elixirTypeConstructor flatten c name args =
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

        ( "Dict", [ key, val ] ) ->
            "%{}"

        -- "%{required("
        --     ++ elixirT flatten c key
        --     ++ ") => "
        --     ++ elixirT flatten c val
        --     ++ "}"
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


typealiasConstructor : List a -> ExContext.Alias -> Maybe Expression
typealiasConstructor args ({ mod, aliasType, arity, body, getTypeBody } as ali) =
    case ( aliasType, body ) of
        ( ExContext.Type, _ ) ->
            Nothing

        ( _, TypeConstructor [ name ] _ ) ->
            Nothing

        ( _, TypeRecord kvs ) ->
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
                Just (Lambda (map (singleton >> Variable) args) (Record varargs))

        -- Error in AST. Single TypeTuple are just paren app
        ( _, TypeTuple [ app ] ) ->
            typealiasConstructor args { ali | getTypeBody = (\_ -> app) }

        ( _, TypeTuple kvs ) ->
            let
                args =
                    List.length kvs
                        |> List.range 1
                        |> List.map (toString >> (++) "arg")
                        |> map (singleton >> Variable)
            in
                Just (Lambda (args) (Tuple args))

        ( _, TypeVariable name ) ->
            Just (Variable [ name ])

        other ->
            Nothing


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
    ExContext.getAlias c.mod name c
        |> Maybe.map
            (\{ mod, getTypeBody, aliasType } ->
                if mod == c.mod then
                    elixirTNoFlat c (getTypeBody args)
                else
                    case aliasType of
                        ExContext.Type ->
                            mod ++ "." ++ elixirTNoFlat c (getTypeBody args)

                        ExContext.TypeAlias ->
                            elixirTNoFlat c (getTypeBody args)
            )
        |> Maybe.withDefault default
