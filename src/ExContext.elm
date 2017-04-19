module ExContext exposing (Context)

import Ast.Statement exposing (ExportSet)


type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    }
