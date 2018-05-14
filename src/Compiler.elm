module Compiler exposing (version, tree)

{-| Module responsible for compiling Elm code to Elixir

@docs version, tree

-}

import Ast
import ExMeta
import ExAlias
import ExStatement
import Dict exposing (Dict)
import Helpers exposing (ind, toSnakeCase)
import Ast.Statement exposing (Statement)
import ExContext exposing (Context)
import Regex exposing (Regex, HowMany(..), regex)


{-| Returns current version
-}
version : String
version =
    "0.7.0"


glueStart : String
glueStart =
    (ind 0) ++ "use Elchemy" ++ "\n"


glueEnd : String
glueEnd =
    "\n"
        ++ String.trim
            """
         end

         """
        ++ "\n"


getName : String -> ( String, String )
getName file =
    case String.split "\n" file of
        n :: rest ->
            ( n, String.join "\n" rest )

        [] ->
            ( "", "" )


{-| Transforms a code in Elm to code in Elixir
-}
tree : String -> String
tree =
    treeAndCommons >> Tuple.first


{-| Transforms a code in Elm to code in Elixir and returns commons
-}
treeAndCommons : String -> ( String, ExContext.Commons )
treeAndCommons m =
    fullTree ExContext.emptyCommons m


{-| Transforms a code in Elm with cache from previous run to code in Elixir and cache
-}
fullTree : ExContext.Commons -> String -> ( String, ExContext.Commons )
fullTree cachedCommons m =
    -- If only no blank characters
    if Regex.contains (Regex.regex "^\\s*$") m then
        ( "", cachedCommons )
    else if not <| String.contains (">>" ++ ">>") m then
        m
            |> parse "NoName.elm"
            |> getContext
            |> (\( c, a ) ->
                    case c of
                        Nothing ->
                            Debug.crash "Failed getting context"

                        Just c ->
                            ( getCode c a, c.commons )
               )
    else
        let
            multiple =
                String.split (">>" ++ ">>") m

            count =
                Debug.log "Number of files" (List.length multiple)

            files =
                multiple
                    |> List.map getName
                    |> List.indexedMap (,)
                    |> List.map
                        (\( i, ( name, code ) ) ->
                            let
                                _ =
                                    flip Debug.log name <|
                                        "Parsing "
                                            ++ toString (count - i)
                                            ++ "/"
                                            ++ toString count
                                            ++ " # "
                            in
                                ( name, parse name code )
                        )

            wContexts =
                files
                    |> List.map (\( name, ast ) -> ( name, getContext ast ))
                    |> List.filterMap
                        (\a ->
                            case a of
                                ( _, ( Nothing, _ ) ) ->
                                    Nothing

                                ( name, ( Just c, ast ) ) ->
                                    Just ( name, c, ast )
                        )

            commons =
                wContexts
                    |> List.map (\( name, ctx, ast ) -> ctx.commons)
                    |> (::) cachedCommons
                    |> getCommonImports
                    |> (\modules -> { modules = modules })

            wTrueContexts =
                wContexts
                    |> List.map (\( name, c, ast ) -> ( name, { c | commons = commons }, ast ))

            compileWithIndex ( i, ( name, c, ast ) ) =
                let
                    _ =
                        flip Debug.log name <|
                            "Compiling "
                                ++ toString (count - i)
                                ++ "/"
                                ++ toString count
                                ++ " # "
                in
                    -- "Used to avoid quadruple > becuase it's a meta string"
                    ">>" ++ ">>" ++ name ++ "\n" ++ getCode c ast
        in
            wTrueContexts
                |> List.indexedMap (,)
                |> List.map compileWithIndex
                |> String.join "\n"
                |> flip (,) commons


getCommonImports : List ExContext.Commons -> Dict String ExContext.Module
getCommonImports commons =
    let
        merge aliases acc =
            Dict.merge Dict.insert (\k v v2 -> Dict.insert k v2) Dict.insert acc aliases Dict.empty
    in
        List.foldl (.modules >> merge) Dict.empty commons


getContext : List Statement -> ( Maybe Context, List Statement )
getContext statements =
    case statements of
        [] ->
            ( Nothing, [] )

        mod :: statements ->
            let
                base =
                    ExStatement.moduleStatement mod
            in
                ( Just (ExAlias.getAliases base statements), statements )


aggregateStatements : Statement -> ( Context, String ) -> ( Context, String )
aggregateStatements s ( c, code ) =
    let
        ( newC, newCode ) =
            ExStatement.elixirS c s
    in
        ( newC, code ++ newCode )


getCode : Context -> List Statement -> String
getCode context statements =
    let
        shadowsBasics =
            ExContext.importBasicsWithoutShadowed context

        ( newC, code ) =
            List.foldl aggregateStatements ( context, "" ) statements
    in
        ("# Compiled using Elchemy v" ++ version)
            ++ "\n"
            ++ ("defmodule " ++ context.mod ++ " do")
            ++ glueStart
            ++ (ind context.indent)
            ++ shadowsBasics
            ++ code
            ++ glueEnd
            ++ ExMeta.metaDefinition { newC | inMeta = True }


parse : String -> String -> List Statement
parse fileName code =
    case Ast.parse (prepare code) of
        Ok ( _, _, statements ) ->
            statements

        Err ( (), { input, position }, [ msg ] ) ->
            let
                ( line, column ) =
                    getLinePosition position code
            in
                Debug.crash <|
                    "]ERR> Parsing error in:\n "
                        ++ fileName
                        ++ ":"
                        ++ toString line
                        ++ ":"
                        ++ toString column
                        ++ "\n"
                        ++ msg
                        ++ "\nat:\n "
                        ++ (input
                                |> String.lines
                                |> List.take 30
                                |> String.join "\n"
                           )
                        ++ "\n"

        err ->
            Debug.crash (toString err)


prepare : String -> String
prepare codebase =
    codebase |> removeComments


removeComments : String -> String
removeComments =
    Regex.replace All (regex " +--.*\\r?\\n") (always "")
        >> Regex.replace All (regex "\\s--.*\\r?\\n") (always "")
        >> Regex.replace All (regex "\n +\\w+ : .*") (always "")


getLinePosition : Int -> String -> ( Int, Int )
getLinePosition character input =
    let
        lines =
            String.slice 0 character input |> String.lines

        line =
            List.length lines

        column =
            List.reverse lines |> List.head |> Maybe.map String.length |> Maybe.withDefault 0
    in
        ( line, column )
