module Compiler exposing (version, tree)

{-| Module responsible for compiling Elm code to Elixir

@docs version, tree

-}

import Ast
import ExAlias
import ExStatement
import Dict exposing (Dict)
import Helpers exposing (ind)
import Ast.Statement exposing (Statement)
import ExContext exposing (Context, Aliases)
import Regex exposing (Regex, HowMany(..), regex)


{-| Returns current version
-}
version : String
version =
    "0.4.45"


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
tree m =
    case String.split (">>" ++ ">>") m of
        [ single ] ->
            single
                |> parse "NoName.elm"
                |> getContext
                |> (\( c, a ) ->
                        case c of
                            Nothing ->
                                Debug.crash "Failed getting context"

                            Just c ->
                                getCode c a
                   )

        multiple ->
            let
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

                commonAliases =
                    wContexts
                        |> List.map (\( name, ctx, ast ) -> ctx.aliases)
                        |> getCommonImports

                commonTypes =
                    wContexts
                        |> List.map (\( name, ctx, ast ) -> ctx.types)
                        |> getCommonImports

                updateWithCommon ( name, c, ast ) =
                    ( name, { c | aliases = commonAliases, types = commonTypes }, ast )

                wTrueContexts =
                    List.map updateWithCommon wContexts

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
                        ">>" ++ ">>" ++ name ++ "\n" ++ getCode c ast
            in
                wTrueContexts
                    |> List.indexedMap (,)
                    |> List.map compileWithIndex
                    |> String.join "\n"


getCommonImports : List (Dict String v) -> Dict String v
getCommonImports a =
    let
        merge aliases acc =
            Dict.merge Dict.insert typeAliasDuplicate Dict.insert acc aliases Dict.empty
    in
        List.foldl merge Dict.empty a


typeAliasDuplicate : comparable -> a -> a -> Dict.Dict comparable a -> Dict.Dict comparable a
typeAliasDuplicate k v v2 =
    if v /= v2 then
        Debug.crash <|
            "You can't have two different type aliases for "
                ++ toString k
                ++ "\nThese are: "
                ++ toString v
                ++ "\nand\n"
                ++ toString v2
    else
        Dict.insert k v


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
    ("# Compiled using Elchemy v" ++ version)
        ++ "\n"
        ++ ("defmodule " ++ context.mod ++ " do")
        ++ glueStart
        ++ ((List.foldl (aggregateStatements) ( context, "" ) statements)
                |> Tuple.second
           )
        ++ glueEnd


parse : String -> String -> List Statement
parse fileName m =
    case Ast.parse (prepare m) of
        Ok ( _, _, statements ) ->
            statements

        Err ( (), { input, position }, [ msg ] ) ->
            Debug.crash <|
                "]ERR> Compilation error in:\n "
                    ++ fileName
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
    -- Need to remove the second one
    Regex.replace All (regex "\\s--.*\n") (always "")
        >> Regex.replace All (regex "\n +\\w+ : .*") (always "")


crunchSplitLines : String -> String
crunchSplitLines =
    Regex.replace All (regex "(?:({-(?:\\n|.)*?-})|([\\w\\])}\"][\\t ]*)\\n[\\t ]+((?!.*\\s->\\s)(?!.*=)(?!.*\\bin\\b)[\\w[({\"]))") <|
        \m ->
            m.submatches
                |> List.map (Maybe.map (flip (++) " "))
                |> List.filterMap identity
                |> String.join " "
