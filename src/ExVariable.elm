module ExVariable exposing (varOrNah, rememberVariables, organizeLetInVariablesOrder)

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


{-| Extracts name of variable used, but only the top level. All of the shadowed
uses will be excluded
-}
extractVariableUses : Expression -> List String
extractVariableUses expression =
    let
        withoutVars vars right =
            List.filter (not << flip List.member vars) (extractVariableUses right)

        rightWithoutLeft left right =
            withoutVars (extractVariables left) right
    in
        case expression of
            Let definitions return ->
                List.concatMap (uncurry rightWithoutLeft) definitions

            Lambda head body ->
                body
                    |> withoutVars (List.concatMap extractVariables head)

            other ->
                extractVariables other


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

            BinOp (Variable [ "as" ]) l ((Variable [ _ ]) as r) ->
                many [ l, r ]

            _ ->
                none


{-| Organize let in variables in order based on how they use each other
Example:
a = b
b = 1
Would become:
b = 1
a = b
-}
organizeLetInVariablesOrder : List ( Expression, Expression ) -> List ( Expression, Expression )
organizeLetInVariablesOrder expressionList =
    let
        compare ( leftVar, leftExp ) ( rightVar, rightExp ) =
            if isIn leftVar rightExp then
                LT
            else if isIn rightVar leftExp then
                GT
            else
                EQ
    in
        List.sortWith compare expressionList


isIn : Expression -> Expression -> Bool
isIn var expression =
    let
        extractName var =
            case var of
                Variable [ name ] ->
                    name

                _ ->
                    ""

        getVarName ( var, _ ) =
            extractName var
    in
        extractVariableUses expression
            |> List.member (extractName var)
