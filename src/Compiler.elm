module Compiler exposing (tree)

import Ast
import Tuple exposing (..)
import List exposing (..)
import Helpers exposing (..)
import ExContext exposing (Context)
import ExStatement
import ExAlias


version =
    "0.0.17"


glueStart : String
glueStart =
    (ind 0)
        ++ "use Elmchemy"
        ++ "\n"


glueEnd : String
glueEnd =
    "\n"
        ++ String.trim
            """
         end
         """


getName : String -> ( String, String )
getName file =
    case String.split "\n" file of
        n :: rest ->
            ( n, String.join "\n" rest )

        [] ->
            ( "", "" )


tree : String -> String
tree m =
    String.split ">>>>" m
        |> List.map getName
        |> List.map
            (\f ->
                Debug.log "Processing" (second f) ++ "\n" ++ parse (second f)
            )
        |> String.join ">>>>"


parse : String -> String
parse m =
    case Ast.parse m of
        Ok ( _, _, mod :: statements ) ->
            let
                base =
                    (ExStatement.moduleStatement mod)

                context =
                    { base | aliases = (ExAlias.getAliases statements) }
            in
                ("# Compiled using Elmchemy v" ++ version)
                    ++ "\n"
                    ++ ("defmodule " ++ context.mod ++ " do")
                    ++ glueStart
                    ++ ((List.map (\a -> ExStatement.elixirS (Debug.log "line" a) context) statements)
                            |> (List.foldr (++) "")
                       )
                    ++ glueEnd

        Err ( (), { input, position }, [ msg ] ) ->
            "]ERR> Compilation error at: "
                ++ input
                ++ "\n"

        err ->
            Debug.crash (toString err)
