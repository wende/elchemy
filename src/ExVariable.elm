module ExVariable exposing (varOrNah, rememberVariables)

import Set
import Ast.Expression exposing (..)
import Helpers exposing (toSnakeCase)
import ExContext exposing (Context, inArgs)


{-| Put variables into context so they are treated like variables in future
-}
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


{-| Check if a string is a variable or no, based on remembered variables
-}
varOrNah : Context -> String -> String
varOrNah c var =
    if Set.member var c.variables || c.inArgs then
        var
    else
        var ++ "()"


{-| Extract variables from an expression
-}
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

            Application left right ->
                many [ left, right ]

            BinOp (Variable [ "::" ]) x xs ->
                many [ x, xs ]

            BinOp (Variable [ "as" ]) ((Variable _) as v1) ((Variable _) as v2) ->
                many [ v1, v2 ]

            BinOp (Variable [ "as" ]) _ (Variable [ name ]) ->
                one name

            _ ->
                none
