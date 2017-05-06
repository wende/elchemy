module ExContext exposing (..)

import Ast.Statement exposing (ExportSet, Type)
import Dict exposing (Dict)


type alias Alias =
    List String -> List Type


type alias Aliases =
    Dict String (List String -> List Type)


type alias Flag =
    ( String, String )


type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    , aliases : Aliases
    , flags : List Flag
    }


indent : Context -> Context
indent c =
    { c | indent = c.indent + 1 }


deindent : Context -> Context
deindent c =
    { c | indent = c.indent - 1 }


addFlag : Flag -> Context -> Context
addFlag flag c =
    { c | flags = flag :: c.flags }


getAllFlags : String -> Context -> List String
getAllFlags key c =
    c.flags
        |> List.filter (Tuple.first >> ((==) key))
        |> List.map Tuple.second


hasFlag : String -> String -> Context -> Bool
hasFlag key value c =
    c.flags
        |> List.any ((==) ( key, value ))
