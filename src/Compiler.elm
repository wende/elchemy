module Compiler exposing (tree)

import Ast
import Tuple exposing (..)
import List exposing (..)
import Helpers exposing (..)
import ExContext exposing (Context)
import ExStatement
import ExAlias


version =
    "0.0.16"


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


tree : String -> String
tree m =
    case Ast.parse m of
        Ok ( _, _, first :: statements ) ->
            let
                base =
                    (ExStatement.moduleStatement first)

                context =
                    { base | aliases = (ExAlias.getAliases statements) }
            in
                ("# Compiled using Elmchemy v" ++ version)
                    ++ "\n"
                    ++ ("defmodule " ++ context.mod ++ " do")
                    ++ glueStart
                    ++ ((List.map (\a -> ExStatement.elixirS a context) statements)
                            |> (List.foldr (++) "")
                       )
                    ++ glueEnd

        Err ( (), { input, position }, [ msg ] ) ->
            "]ERR> Compilation error at: "
                ++ input
                ++ "\n"

        err ->
            Debug.crash (toString err)
