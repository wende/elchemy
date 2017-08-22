module ExVariable exposing (..)

import Ast.Expression exposing (..)
import ExContext exposing (Context, inArgs)
import Set
import Helpers exposing (toSnakeCase)


rememberVariables : List Expression -> Context -> Context
rememberVariables list c =
    let
        addToContext var context =
            { context
                | variables = Set.insert (toSnakeCase True var) (context.variables)
            }
    in
        list
            |> List.map (extractVariables)
            |> List.foldr (++) []
            |> List.foldl addToContext c


varOrNah : Context -> String -> String
varOrNah c var =
    if Set.member var c.variables || c.inArgs then
        var
    else
        var ++ "()"


extractVariables : Expression -> List String
extractVariables exp =
    let
        many vars =
            vars
                |> List.map extractVariables
                |> List.foldr (++) []

        one var =
            [ var ]

        none =
            []
    in
        case exp of
            Record vars ->
                vars
                    |> List.map (Tuple.second)
                    |> many

            Tuple vars ->
                many vars

            Variable [ name ] ->
                one name

            List vars ->
                many vars

            Application _ right ->
                many [ right ]

            BinOp (Variable [ "::" ]) x xs ->
                many [ x, xs ]

            BinOp (Variable [ "as" ]) ((Variable _) as v1) ((Variable _) as v2) ->
                many [ v1, v2 ]

            BinOp (Variable [ "as" ]) _ (Variable [ name ]) ->
                one name

            _ ->
                none
