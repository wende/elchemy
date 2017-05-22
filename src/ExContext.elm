module ExContext exposing (..)

import Ast.Statement exposing (ExportSet, Type)
import Dict exposing (Dict)


type alias Aliases =
    Dict String ( String, Type )


type alias Flag =
    ( String, String )


type alias Definition =
    { arity : Int, def : Type }


type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    , aliases : Aliases
    , flags : List Flag
    , definitions : Dict String Definition
    }


empty : String -> ExportSet -> Context
empty name exports =
    Context name exports 0 Dict.empty [] Dict.empty


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
