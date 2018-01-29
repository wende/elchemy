module ExSelector exposing (Selector(..), maybeAccessMacro, AccessMacro(..), AccessMacroType(..))

import Ast.Expression exposing (Expression(AccessFunction, Application, Variable))
import Regex
import Char
import List.Extra
import Helpers


type Selector
    = Access String


type AccessMacroType
    = Get
    | Put
    | Update


type AccessMacro
    = AccessMacro AccessMacroType Int (List Selector)


getSelector : Expression -> Selector
getSelector expression =
    case expression of
        AccessFunction name ->
            Access (Helpers.toSnakeCase True name)

        _ ->
            Debug.crash "The only allowed selectors are: .field"


maybeAccessMacro : Expression -> List Expression -> Maybe ( AccessMacro, List Expression )
maybeAccessMacro call args =
    let
        accessMacroArgs arity args =
            case compare (List.length args) arity of
                LT ->
                    Debug.crash <|
                        "Access macros [updateIn/getIn/putIn] cannot be partially applied. Expecting "
                            ++ toString arity
                            ++ " selector arguments."

                EQ ->
                    ( List.map getSelector args, [] )

                GT ->
                    List.Extra.splitAt arity args
                        |> Tuple.mapFirst (List.map getSelector)
    in
        case ( call, args ) of
            ( Variable [ name ], args ) ->
                accessMacroType name
                    |> Maybe.map
                        (\( t, arity ) ->
                            let
                                ( selectors, rest ) =
                                    accessMacroArgs arity args
                            in
                                ( AccessMacro t arity selectors, rest )
                        )

            _ ->
                Nothing


accessMacroType : String -> Maybe ( AccessMacroType, Int )
accessMacroType string =
    let
        getArity =
            String.filter Char.isDigit
                >> String.toInt
                >> Result.withDefault 1

        getType x =
            [ ( "updateIn\\d?", Update )
            , ( "putIn\\d?", Put )
            , ( "getIn\\d?", Get )
            ]
                |> List.foldl
                    (\( match, res ) acc ->
                        case acc of
                            Nothing ->
                                if Regex.contains (Regex.regex match) x then
                                    Just res
                                else
                                    Nothing

                            res ->
                                res
                    )
                    Nothing
    in
        getType string
            |> Maybe.map (\t -> ( t, getArity string ))
