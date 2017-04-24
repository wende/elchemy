module ExContext exposing (..)

import Ast.Statement exposing (ExportSet, Type)
import Dict exposing (Dict)


type alias Aliases =
    Dict String Type


type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    , aliases : Aliases
    }
