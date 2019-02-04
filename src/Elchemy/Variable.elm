module Elchemy.Variable
    exposing
        ( extractName
        , groupByCrossDependency
        , organizeLetInVariablesOrder
        , rememberVariables
        , varOrNah
        )

import Ast.Expression exposing (..)
import Elchemy.Context as Context exposing (Context, inArgs)
import Elchemy.Helpers as Helpers exposing (toSnakeCase)
import List.Extra
import Set


{-| Put variables into context so they are treated like variables in future
-}
rememberVariables : List Expression -> Context -> Context
rememberVariables list c =
    let
        addToContext var context =
            { context
                | variables = Set.insert (toSnakeCase True var) context.variables
            }
    in
        list
            |> List.map extractVariablesUsed
            |> List.foldr (++) []
            |> List.foldl addToContext c


{-| Check if a string is a variable or no, based on remembered variables
-}
varOrNah : Context -> String -> String
varOrNah c var =
    if Set.member var c.variables || c.inArgs then
        var
    else if c.inMeta then
        c.mod ++ "." ++ var ++ "()"
    else
        var ++ "()"


{-| Extract variables from an expression
-}
extractVariablesUsed : Expression -> List String
extractVariablesUsed exp =
    let
        many vars =
            vars
                |> List.map extractVariablesUsed
                |> List.foldr (++) []

        one var =
            [ var ]

        none =
            []
    in
        case exp of
            Record vars ->
                vars
                    |> List.map Tuple.second
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

            BinOp _ l r ->
                many [ l, r ]

            -- Assignments
            Case head branches ->
                List.concatMap (uncurry rightWithoutLeft) branches
                    |> withoutVars (extractVariablesUsed head)

            Let definitions return ->
                List.concatMap (uncurry rightWithoutLeft) definitions

            Lambda head body ->
                extractVariablesUsed body
                    |> withoutVars (List.concatMap extractVariablesUsed head)

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
organizeLetInVariablesOrder : Context -> List ( Expression, Expression ) -> List ( Expression, Expression )
organizeLetInVariablesOrder c expressionList =
    case bubbleSelect (\a b -> not <| isIn a b) expressionList of
        Ok list ->
            list

        Err list ->
            let
                _ =
                    Context.crash c <|
                        "Couldn't find a solution to "
                            ++ toString (list |> List.map Tuple.first)
            in
                []


{-| Returns a name of a variable, or a name of a function being applied
-}
extractName : Context -> Expression -> String
extractName c expression =
    case Helpers.applicationToList expression of
        [ Variable [ name ] ] ->
            name

        [ single ] ->
            Context.crash c (toString single ++ " is not a variable")

        multi ->
            List.head multi
                |> Maybe.map (extractName c)
                |> Maybe.withDefault ""


{-| Returns a list of names of a variable, or a name of a function being applied

a = 1 --> ["a"]
f a b c = 1 --> ["f"]
(a, b, c) = 1 --> ["a", "b", "c"]

-}
extractNamesAssigned : Expression -> List String
extractNamesAssigned expression =
    case Helpers.applicationToList expression of
        [ Variable [ name ] ] ->
            [ name ]

        [ single ] ->
            extractVariablesUsed single

        multi ->
            List.head multi
                |> Maybe.map extractNamesAssigned
                |> Maybe.withDefault []


{-| Extracts only the arguments of an expression (if the expression is a function)
-}
extractArguments : Expression -> List String
extractArguments expression =
    case Helpers.applicationToList expression of
        [ single ] ->
            []

        multi ->
            List.tail multi
                |> Maybe.map (List.concatMap extractNamesAssigned)
                |> Maybe.withDefault []


{-| Returns true if a name of right argument is mentioned in a body of left variable
-}
isIn : ( Expression, Expression ) -> ( Expression, Expression ) -> Bool
isIn ( leftHead, _ ) ( rightHead, rightDef ) =
    let
        anyMembers members list =
            List.any (flip List.member list) members
    in
        withoutVars (extractArguments rightHead) (extractVariablesUsed rightDef)
            |> anyMembers (extractNamesAssigned leftHead)


{-| Returns a list of variables in right, without these passed as first argument}
-}
withoutVars : List String -> List String -> List String
withoutVars vars right =
    List.filter (not << flip List.member vars) right


{-| Returns a list of variables used in right expression, without variables defined in
left expression
-}
rightWithoutLeft : Expression -> Expression -> List String
rightWithoutLeft left right =
    withoutVars (extractVariablesUsed left) (extractVariablesUsed right)


{-| Groups functions that mutually call each other in lists
-}
groupByCrossDependency : List ( Expression, Expression ) -> List (List ( Expression, Expression ))
groupByCrossDependency expressionsList =
    expressionsList
        |> List.Extra.groupWhile (\l r -> isIn l r && isIn r l)


{-| Selects a correct order of elements in a list based on a dependency algorithm
if dependency requirement (f) is met for all of the other elements it is inserted
otherwise it looks for next element that fits the requirement.
Function returns first combination found, satifying the predicate.
If no function was found it returns Nothing
-}
bubbleSelect : (a -> a -> Bool) -> List a -> Result (List a) (List a)
bubbleSelect f list =
    let
        findIndex discarded list =
            List.Extra.break (\a -> List.all (f a) discarded) list

        findNext discarded list acc =
            case list of
                [] ->
                    if discarded == [] then
                        Ok <| List.reverse acc
                    else
                        Err discarded

                -- Trick to allow mutual recursion
                -- case findIndex discarded acc of
                --     ( l, r ) ->
                --         Ok <| l ++ (List.reverse discarded) ++ r
                current :: tail ->
                    let
                        newlist =
                            discarded ++ tail
                    in
                        if List.all (flip f current) newlist then
                            findNext [] newlist (current :: acc)
                        else
                            findNext (current :: discarded) tail acc
    in
        findNext [] list []
