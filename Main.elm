module Main exposing (main)

import Html.Events exposing (..)
import Json.Decode as JD
import Html exposing (..)
import Html
import Html.Attributes
import Compiler
import Markdown


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
        , Markdown.toHtml [] <| "```elixir\n" ++ (Compiler.tree model) ++ "\n```"
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
    """module Stack exposing (..)

import Elmchemy exposing (..)


meta =
    [ "use GenServer" ]


type alias State a =
    List a


type GenServerReturn a b
    = Reply a (State b)
    | NoReply (State b)


type Command a
    = Stack
    | Push a
    | Pop



-- Client


startLink : a -> Pid
startLink default =
    ffi "GenServer" "start_link" ( Stack, default )


push : Pid -> a -> a
push pid item =
    ffi "GenServer" "cast" ( pid, (Push item) )


pop : Pid -> a
pop pid =
    ffi "GenServer" "call" ( pid, Pop )



-- Server (callbacks)


handle_call : Command a -> Pid -> State a -> GenServerReturn a a
handle_call command from state =
    case ( command, from, state ) of
        Pop, _, (h :: t) ->
            Reply h t

        ( request, from, state ) ->
            lffi "super" ( request, from, state )


handle_cast : Command a -> State a -> GenServerReturn a a
handle_cast command state =
    case ( command, state ) of
        (Push item), state ->
            NoReply (item :: state)

        ( request, state ) ->
            lffi "super" ( request, state )
"""


type Czlowiek a
    = List a


type Wende
    = Czlowiek Int


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m

        String ->
            ""
