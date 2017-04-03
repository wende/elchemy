module Main exposing (main)
import Html.Events exposing (..)
import Json.Decode as JD
import Html exposing (..)
import Html
import Html.Attributes

import Compiler

type Msg
    = Replace String | String


divStyle : Attribute msg
divStyle =
    Html.Attributes.style
    [ ("display", "inline-flex")
    , ("width", "100%")
    ]

codeStyle : Attribute msg
codeStyle =
    Html.Attributes.style
    [("width", "50%")
    ,("margin", "10px")]


view : String -> Html Msg
view model =
    div [divStyle]
        [ textarea [ codeStyle, on "input" (JD.map Replace targetValue) ] [ text model ]
        ,  pre [codeStyle] [text (Compiler.tree model)]
        ]


main : Program Never String Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

init : String
init =
    """module Main exposing (..)
import Elixir.Glue exposing (..)
f : Int -> Int
f x = x + 1
add : Int -> Int -> Int
add a b = a + Just b
h = wende g + hajto cichocinski 10
casa t =
    case t of
        wende -> 1
        cichocinski ->
            \\a -> 1 + 2
-- (Notice that record types use colon and record values use equals.)

-- You can give existing types a nice name with a type alias.
type alias Point3D =
  { x : Float, y : Float, z : Float }

-- If you alias a record, you can use the name as a constructor function.
otherOrigin : Point3D
otherOrigin =
  Point3D 0 0 0

-- But it's still the same type, so you can equate them.

-- By contrast, defining a union type creates a type that didn't exist before.
-- A union type is so called because it can be one of many possibilities.
-- Each of the possibilities is represented as a "tag".
type Direction =
  North | South | East | West

-- Tags can carry other values of known type. This can work recursively.
type IntTree =
  Leaf | Node Int IntTree IntTree
-- "Leaf" and "Node" are the tags. Everything following a tag is a typ

-- Pattern match union tags. The uppercase tags will be matched exactly. The
-- lowercase variables will match anything. Underscore also matches anything,


"""
update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m
        String ->
            ""
