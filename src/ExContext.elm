module ExContext exposing (..)

import Ast.Statement exposing (ExportSet, Type(..))
import Dict exposing (Dict)
import Set exposing (Set)


type alias Aliases =
    Dict String Alias


type AliasType
    = Type
    | TypeAlias


type alias Alias =
    { mod : String
    , arity : Int
    , aliasType : AliasType
    , body : Type
    , getTypeBody : List Type -> Type
    }


noParamAlias : Type -> (List Type -> Type)
noParamAlias return params =
    case ( return, params ) of
        ( _, [] ) ->
            return

        ( TypeVariable name, other ) ->
            wrongArityAlias 0 other name

        other ->
            Debug.crash ("Wrong alias defintion " ++ toString other)


wrongArityAlias : Int -> List Type -> String -> a
wrongArityAlias arity list name =
    "Expected "
        ++ toString arity
        ++ " arguments for "
        ++ name
        ++ ". But got "
        ++ (toString <| List.length list)
        |> Debug.crash


type alias Flag =
    ( String, String )


type alias Definition =
    { arity : Int, def : Type }


type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    , aliases : Aliases
    , types : Dict String Int
    , flags : List Flag
    , definitions : Dict String Definition
    , variables : Set String
    , inArgs : Bool
    }


empty : String -> ExportSet -> Context
empty name exports =
    Context name exports 0 Dict.empty Dict.empty [] Dict.empty Set.empty False


indent : Context -> Context
indent c =
    { c | indent = c.indent + 1 }


deindent : Context -> Context
deindent c =
    { c | indent = c.indent - 1 }


addFlag : Flag -> Context -> Context
addFlag flag c =
    { c | flags = flag :: c.flags }


onlyWithoutFlag : Context -> String -> String -> String -> String
onlyWithoutFlag c key value code =
    if hasFlag key value c then
        ""
    else
        code


getAllFlags : String -> Context -> List String
getAllFlags key c =
    c.flags
        |> List.filter (Tuple.first >> ((==) key))
        |> List.map Tuple.second


hasFlag : String -> String -> Context -> Bool
hasFlag key value c =
    c.flags
        |> List.any ((==) ( key, value ))


inArgs : Context -> Context
inArgs c =
    { c | inArgs = True }


mergeVariables : Context -> Context -> Context
mergeVariables left right =
    { left | variables = Set.union left.variables right.variables }
