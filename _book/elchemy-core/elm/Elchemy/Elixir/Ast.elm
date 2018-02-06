module Elchemy.Elixir.Ast exposing (..)


type Expression
    = Application Expression (List Expression)
    | Variable String
    | Do (List Expression)
