port module Main exposing (main)

import Html exposing (..)
import Html
import Compiler
import Markdown


type Msg
    = Replace String
    | String


view : String -> Html Msg
view model =
    Markdown.toHtml [] <| "```elixir\n" ++ (Compiler.tree model) ++ "\n```"


init : String -> ( String, Cmd Msg )
init value =
    ( value, Cmd.none )


main : Program String String Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.batch [ updateInput Replace ])
        }


update : Msg -> String -> ( String, Cmd Msg )
update action model =
    case action of
        Replace m ->
            ( m, Cmd.none )

        String ->
            ( "", Cmd.none )


port updateInput : (String -> msg) -> Sub msg
