module ExStatement exposing (..)

import Ast
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Ast.BinOp exposing (operators)
import ExContext exposing (Context, Definition, indent, deindent, onlyWithoutFlag)
import ExExpression
import ExType
import Helpers exposing (..)
import List exposing (..)
import Dict exposing (Dict)
import Regex exposing (..)
import Helpers exposing (..)
import Debug exposing (crash)


type ElchemyComment
    = Doc String
    | Ex String
    | Normal String
    | Flag String


type DocType
    = Fundoc
    | Typedoc
    | ModuleDoc


moduleStatement : Statement -> Context
moduleStatement s =
    case s of
        ModuleDeclaration names exports ->
            ExContext.empty (String.join "." names) exports

        other ->
            crash "First statement must be module declaration"


elixirS : Context -> Statement -> ( Context, String )
elixirS c s =
    case s of
        InfixDeclaration _ _ _ ->
            ( c, "" )

        TypeDeclaration (TypeConstructor [ name ] _) types ->
            let
                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Typedoc)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) newC <|
                    code
                        ++ (ind c.indent)
                        ++ "@type "
                        ++ toSnakeCase True name
                        ++ " :: "
                        ++ (map (ExType.uniontype c) types |> String.join " | ")
                        ++ "\n"

        TypeAliasDeclaration _ _ ->
            ( c, "" )

        (FunctionTypeDeclaration name ((TypeApplication _ _) as t)) as def ->
            let
                definition =
                    getTypeDefinition def

                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) (addTypeDefinition newC name definition) <|
                    (onlyWithoutFlag newC "nodef" name code)
                        ++ case isOperator name of
                            Builtin ->
                                -- TODO implement operator specs
                                ""

                            Custom ->
                                -- onlyWithoutFlag c
                                -- "nospec0"
                                -- name
                                -- ((ind c.indent)
                                --     ++ "@spec "
                                --     ++ translateOperator name
                                --     ++ (ExType.typespec0 c t)
                                -- )
                                onlyWithoutFlag newC
                                    "nospec"
                                    name
                                    ((ind newC.indent)
                                        ++ "@spec "
                                        ++ translateOperator name
                                        ++ (ExType.typespec newC t)
                                    )

                            None ->
                                -- onlyWithoutFlag c
                                --     "nospec0"
                                --     name
                                --     ((ind c.indent)
                                --         ++ "@spec "
                                --         ++ toSnakeCase True name
                                --         ++ (ExType.typespec0 c t)
                                --     )
                                onlyWithoutFlag newC
                                    "nospec"
                                    name
                                    ((ind newC.indent)
                                        ++ "@spec "
                                        ++ toSnakeCase True name
                                        ++ (ExType.typespec newC t)
                                    )

        (FunctionTypeDeclaration name t) as def ->
            let
                definition =
                    getTypeDefinition def

                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) (addTypeDefinition newC name definition) <|
                    code
                        ++ case isOperator name of
                            Builtin ->
                                -- TODO implement operator specs
                                ""

                            Custom ->
                                onlyWithoutFlag newC
                                    name
                                    "nospec"
                                    ((ind c.indent)
                                        ++ "@spec "
                                        ++ translateOperator name
                                        ++ (ExType.typespec newC t)
                                    )

                            None ->
                                onlyWithoutFlag newC
                                    name
                                    "nospec"
                                    ((ind c.indent)
                                        ++ "@spec "
                                        ++ toSnakeCase True name
                                        ++ (ExType.typespec newC t)
                                    )

        (FunctionDeclaration name args body) as fd ->
            (,) c <|
                if name == "meta" && args == [] then
                    ExExpression.generateMeta body
                else
                    case body of
                        (Application (Application (Variable [ "ffi" ]) _) _) as app ->
                            ExExpression.generateFfi
                                c
                                name
                                (c.definitions
                                    |> Dict.get name
                                    |> Maybe.map
                                        (.def
                                            >> typeAplicationToList
                                        )
                                    |> Maybe.withDefault []
                                    |> map typeAplicationToList
                                )
                                app

                        (Application (Application (Variable [ "tryFfi" ]) _) _) as app ->
                            ExExpression.generateFfi
                                c
                                name
                                (c.definitions
                                    |> Dict.get name
                                    |> Maybe.map
                                        (.def
                                            >> typeAplicationToList
                                        )
                                    |> Maybe.withDefault []
                                    |> map typeAplicationToList
                                )
                                app

                        Case vars expressions ->
                            if ExExpression.flattenCommas vars == args then
                                ExExpression.genOverloadedFunctionDefinition
                                    c
                                    name
                                    args
                                    body
                                    expressions
                            else
                                ExExpression.genFunctionDefinition
                                    c
                                    name
                                    args
                                    body

                        _ ->
                            ExExpression.genFunctionDefinition
                                c
                                name
                                args
                                body

        Comment content ->
            case getCommentType content of
                Doc content ->
                    if c.hasModuleDoc then
                        (,) { c | lastDoc = Just content } ""
                    else
                        elixirDoc c ModuleDoc content

                Ex content ->
                    (,) c <|
                        (content
                            |> String.split "\n"
                            |> map String.trim
                            |> String.join "\n"
                            |> indAll c.indent
                        )

                Flag content ->
                    flip (,) "" <|
                        (content
                            |> Regex.split All (regex "\\s+")
                            |> map (String.split ":+")
                            |> filterMap
                                (\flag ->
                                    case flag of
                                        [ k, v ] ->
                                            Just ( k, v )

                                        [ "" ] ->
                                            Nothing

                                        a ->
                                            crash ("Wrong flag format " ++ toString a)
                                )
                            |> foldl (ExContext.addFlag) c
                        )

                Normal content ->
                    (,) c <|
                        (content
                            |> prependAll ("# ")
                            |> indAll c.indent
                        )

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            (,) c <|
                (ind c.indent)
                    ++ "alias "
                    ++ modulePath path

        ImportStatement path (Just asName) Nothing ->
            (,) c <|
                (ind c.indent)
                    ++ "alias "
                    ++ modulePath path
                    ++ ", as: "
                    ++ asName

        ImportStatement path Nothing (Just (SubsetExport exports)) ->
            (,) c <|
                (ind c.indent)
                    ++ "import "
                    ++ modulePath path
                    ++ ", only: ["
                    ++ (map subsetExport exports |> foldr (++) [] |> String.join ",")
                    ++ "]"

        -- Suppresses the compiler warning
        ImportStatement [ "Elchemy" ] Nothing (Just AllExport) ->
            ( c, "" )

        ImportStatement path Nothing (Just AllExport) ->
            (,) c <|
                (ind c.indent)
                    ++ "import "
                    ++ modulePath path

        s ->
            (,) c <|
                notImplemented "statement" s


elixirDoc : Context -> DocType -> String -> ( Context, String )
elixirDoc c doctype content =
    let
        prefix =
            if not c.hasModuleDoc then
                "@moduledoc"
            else if doctype == Fundoc then
                "@doc"
            else
                "@typedoc"
    in
        (,)
            { c
                | hasModuleDoc = True
                , lastDoc = Nothing
            }
        <|
            (ind c.indent)
                ++ prefix
                ++ " \"\"\"\n "
                ++ (content
                        |> String.lines
                        |> map (maybeDoctest c)
                        |> map (Helpers.escape)
                        -- |> map trimIndentations
                        |> String.join (ind c.indent)
                    -- Drop an unnecessary \n at the end
                   )
                ++ ind c.indent
                ++ "\"\"\""


getCommentType : String -> ElchemyComment
getCommentType comment =
    [ ( "^\\sex\\b", (Ex) )
    , ( "^\\|", (Doc) )
    , ( "^\\sflag\\b", (Flag) )
    ]
        |> List.map (\( a, b ) -> ( Regex.regex a, b ))
        |> List.foldl findCommentType (Normal comment)


findCommentType : ( Regex.Regex, String -> ElchemyComment ) -> ElchemyComment -> ElchemyComment
findCommentType ( regex, commentType ) acc =
    case acc of
        Normal content ->
            if Regex.contains regex content then
                commentType <|
                    Regex.replace (Regex.AtMost 1) regex (always "") content
            else
                Normal content

        other ->
            other


subsetExport : ExportSet -> List String
subsetExport exp =
    case exp of
        TypeExport _ _ ->
            []

        FunctionExport name ->
            [ "{:'" ++ name ++ "', 0}" ]

        _ ->
            crash ("You can't export " ++ toString exp)


maybeDoctest : Context -> String -> String
maybeDoctest c line =
    if String.startsWith (ind (c.indent + 1)) ("\n" ++ line) then
        case Ast.parseExpression Ast.BinOp.operators (String.trim line) of
            Ok ( _, _, BinOp (Variable [ "==" ]) l r ) ->
                --"\n"
                indNoNewline (c.indent + 2)
                    ++ "iex> import "
                    ++ c.mod
                    ++ ind (c.indent + 2)
                    ++ "iex> "
                    ++ ExExpression.elixirE c l
                    ++ ind (c.indent + 2)
                    ++ ExExpression.elixirE c r
                    ++ "\n"

            _ ->
                line
    else
        line


getTypeDefinition : Statement -> Definition
getTypeDefinition a =
    case a of
        FunctionTypeDeclaration name t ->
            let
                arity =
                    typeAplicationToList t |> length
            in
                Definition (arity - 1) t

        _ ->
            Debug.crash "It's not a type declaration"


addTypeDefinition : Context -> String -> Definition -> Context
addTypeDefinition c name d =
    { c
        | definitions =
            Dict.insert name
                d
                c.definitions
    }


typeAplicationToList : Type -> List Type
typeAplicationToList application =
    case application of
        TypeApplication left right ->
            [ left ] ++ typeAplicationToList right

        other ->
            [ other ]
