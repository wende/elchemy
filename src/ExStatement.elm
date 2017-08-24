module ExStatement exposing (..)

import Ast
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Ast.BinOp exposing (operators)
import ExContext exposing (Context, Definition, indent, deindent, onlyWithoutFlag)
import ExExpression
import ExType
import List exposing (..)
import Dict exposing (Dict)
import Regex exposing (..)
import Helpers exposing (..)
import ExFfi
import ExFunction


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
        ModuleDeclaration path exports ->
            ExContext.empty (modulePathName path) exports

        other ->
            Debug.crash "First statement must be module declaration"


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
                        ++ case operatorType name of
                            Builtin ->
                                -- TODO implement operator specs
                                ""

                            Custom ->
                                onlyWithoutFlag newC "nospec" name <|
                                    (ind newC.indent)
                                        ++ "@spec "
                                        ++ translateOperator name
                                        ++ (ExType.typespec newC t)

                            None ->
                                onlyWithoutFlag newC "nospec" name <|
                                    (ind newC.indent)
                                        ++ "@spec "
                                        ++ toSnakeCase True name
                                        ++ (ExType.typespec newC t)

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
                        ++ case operatorType name of
                            Builtin ->
                                -- TODO implement operator specs
                                ""

                            Custom ->
                                onlyWithoutFlag newC name "nospec" <|
                                    (ind c.indent)
                                        ++ "@spec "
                                        ++ translateOperator name
                                        ++ (ExType.typespec newC t)

                            None ->
                                onlyWithoutFlag newC name "nospec" <|
                                    (ind c.indent)
                                        ++ "@spec "
                                        ++ toSnakeCase True name
                                        ++ (ExType.typespec newC t)

        (FunctionDeclaration name args body) as fd ->
            let
                genFfi =
                    ExFfi.generateFfi c ExExpression.elixirE name <|
                        (c.definitions
                            |> Dict.get name
                            |> Maybe.map
                                (.def
                                    >> typeAplicationToList
                                )
                            |> Maybe.withDefault []
                            |> map typeAplicationToList
                        )
            in
                c
                    => if
                        Dict.get name c.definitions
                            == Nothing
                            && not (ExContext.isPrivate c name)
                       then
                        Debug.crash <|
                            "To be able to export it, you need to provide function type for `"
                                ++ name
                                ++ "` function in module "
                                ++ toString c.mod
                       else
                        case body of
                            (Application (Application (Variable [ "ffi" ]) _) _) as app ->
                                genFfi app

                            (Application (Application (Variable [ "tryFfi" ]) _) _) as app ->
                                genFfi app

                            Case vars expressions ->
                                if ExFunction.flattenCommas vars == args then
                                    ExFunction.genOverloadedFunctionDefinition c ExExpression.elixirE name args body expressions
                                else
                                    ExFunction.genFunctionDefinition c ExExpression.elixirE name args body

                            _ ->
                                ExFunction.genFunctionDefinition c ExExpression.elixirE name args body

        Comment content ->
            elixirComment c content

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            c
                => (ind c.indent)
                ++ "alias "
                ++ modulePath path

        ImportStatement path (Just asName) Nothing ->
            c
                => (ind c.indent)
                ++ "alias "
                ++ modulePath path
                ++ ", as: "
                ++ asName

        ImportStatement path Nothing (Just ((SubsetExport exports) as subset)) ->
            ExContext.mergeTypes subset (modulePathName path) c
                => (ind c.indent)
                ++ "import "
                ++ modulePath path
                ++ ", only: ["
                ++ (map subsetExport exports |> foldr (++) [] |> String.join ",")
                ++ "]"

        -- Suppresses the compiler warning
        ImportStatement [ "Elchemy" ] Nothing (Just AllExport) ->
            ( c, "" )

        ImportStatement path Nothing (Just AllExport) ->
            ExContext.mergeTypes AllExport (modulePathName path) c
                => (ind c.indent)
                ++ "import "
                ++ modulePath path

        s ->
            (,) c <|
                notImplemented "statement" s


verifyFlag : List String -> Maybe ( String, String )
verifyFlag flag =
    case flag of
        [ k, v ] ->
            Just ( k, v )

        [ "" ] ->
            Nothing

        a ->
            Debug.crash <| "Wrong flag format " ++ toString a


elixirComment : Context -> String -> ( Context, String )
elixirComment c content =
    case getCommentType content of
        Doc content ->
            if c.hasModuleDoc then
                { c | lastDoc = Just content } => ""
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
                    |> filterMap verifyFlag
                    |> foldl (ExContext.addFlag) c
                )

        Normal content ->
            (,) c <|
                (content
                    |> prependAll ("# ")
                    |> indAll c.indent
                )


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
                        |> map (Regex.replace All (regex "\"\"\"") (always "\\\"\\\"\\\""))
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
        |> List.foldl (uncurry findCommentType) (Normal comment)


findCommentType : Regex.Regex -> (String -> ElchemyComment) -> ElchemyComment -> ElchemyComment
findCommentType regex commentType acc =
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
            if isCustomOperator name then
                [ "{:'" ++ translateOperator name ++ "', 0}" ]
            else
                [ "{:'" ++ toSnakeCase True name ++ "', 0}" ]

        _ ->
            Debug.crash ("You can't export " ++ toString exp)


maybeDoctest : Context -> String -> String
maybeDoctest c line =
    if String.startsWith (ind (c.indent + 1)) ("\n" ++ line) then
        case Ast.parseExpression Ast.BinOp.operators (String.trim line) of
            Ok ( _, _, BinOp (Variable [ "==" ]) l r ) ->
                --"\n"
                indNoNewline (c.indent + 1)
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
    { c | definitions = Dict.insert name d c.definitions }


typeAplicationToList : Type -> List Type
typeAplicationToList application =
    case application of
        TypeApplication left right ->
            [ left ] ++ typeAplicationToList right

        other ->
            [ other ]
