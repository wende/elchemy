module Main exposing (main)

import Html.Events exposing (..)
import Json.Decode as JD
import Html exposing (..)
import Html
import Html.Attributes
import Compiler


type Msg
    = Replace String
    | String


divStyle : Attribute msg
divStyle =
    Html.Attributes.style
        [ ( "display", "inline-flex" )
        , ( "width", "100%" )
        ]


codeStyle : Attribute msg
codeStyle =
    Html.Attributes.style
        [ ( "width", "50%" )
        , ( "margin", "10px" )
        , ( "font-family", "monospace" )
        , ( "font-size", "13px" )
        ]


view : String -> Html Msg
view model =
    div [ divStyle ]
        [ textarea [ codeStyle, on "input" (JD.map Replace targetValue) ] [ text model ]
        , pre [ codeStyle ] [ text (Compiler.tree model) ]
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

tupleAdd : (number, number) -> number
tupleAdd tuple = first tuple + second tuple

add : number -> number -> number
add a b = a + Just b

h = wende g + hajto cichocinski 10

casa t =
    case t of
        wende -> 1
        cichocinski ->
            \\a -> 1 + 2
-- If you alias a record, you can use the name as a constructor function.
otherOrigin : Point3D Int Int Int
otherOrigin =
    Point3D 0 0 0

justPoint : Point3D
justPoint =
    Point3D.struct

add a b =
    case a of
        1 -> a
        _ -> b

"""


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m

        String ->
            ""
