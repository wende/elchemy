module ExStatement exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import ExContext exposing (Context, indent, deindent)
import ExExpression
import ExType
import Helpers exposing (..)
import List exposing (..)
import Dict exposing (Dict)
import Regex
import Helpers exposing (..)


type ElmchemyComment
    = Doc String
    | Ex String
    | Normal String


moduleStatement : Statement -> Context
moduleStatement s =
    case s of
        ModuleDeclaration names exports ->
            Context (String.join "." names) exports 0 Dict.empty

        other ->
            Debug.crash "First statement must be module declaration"


elixirS : Context -> Statement -> String
elixirS c s =
    case s of
        TypeDeclaration (TypeConstructor [ name ] _) types ->
            (ind c.indent)
                ++ "@type "
                ++ toSnakeCase name
                ++ " :: "
                ++ ((map (ExType.typealias c) types) |> String.join " | ")

        TypeAliasDeclaration _ _ ->
            ""

        --"alias?"
        FunctionTypeDeclaration name t ->
            if isOperator name then
                -- TODO implement operator specs
                ""
            else
                (ind c.indent)
                    ++ "@spec "
                    ++ toSnakeCase name
                    ++ (ExType.typespec c t)
                    ++ (ind c.indent)
                    ++ "@spec "
                    ++ toSnakeCase name
                    ++ (ExType.typespec0 c t)

        (FunctionDeclaration name args body) as fd ->
            if name == "meta" && args == [] then
                ExExpression.generateMeta body
            else
                case body of
                    Case (Variable _) expressions ->
                        ExExpression.genOverloadedFunctionDefinition
                            c
                            name
                            args
                            body
                            expressions

                    Case nonVar expressions ->
                        if
                            ExExpression.flattenCommas nonVar
                                == map (\a -> Variable [ a ]) args
                        then
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
                    (ind c.indent)
                        ++ "@doc \"\"\"\n"
                        ++ indentComment c content
                        ++ (ind c.indent)
                        ++ "\"\"\""

                Ex content ->
                    content
                        |> String.split "\n"
                        |> map String.trim
                        |> String.join "\n"
                        |> indAll c.indent

                Normal content ->
                    content
                        |> prependAll ((ind c.indent) ++ "# ")

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            (ind c.indent) ++ "alias " ++ String.join "." path

        ImportStatement path Nothing (Just (SubsetExport exports)) ->
            (ind c.indent)
                ++ "import "
                ++ String.join "." path
                ++ ", only: "
                ++ (map subsetExport exports |> foldl (++) [] |> String.join ",")

        ImportStatement path Nothing (Just AllExport) ->
            (ind c.indent) ++ "import " ++ String.join "." path

        s ->
            notImplemented "statement" s


indentComment : Context -> String -> String
indentComment { indent } content =
    content
        |> indAll indent
        -- Drop an unnecessary \n at the end
        |> String.dropRight 1


getCommentType : String -> ElmchemyComment
getCommentType comment =
    [ ( "^\\sex", (\s -> Ex s) )
    , ( "^\\|\\s", (\s -> Doc s) )
    ]
        |> List.map (\( a, b ) -> ( Regex.regex a, b ))
        |> List.foldl findCommentType (Normal comment)


findCommentType : ( Regex.Regex, String -> ElmchemyComment ) -> ElmchemyComment -> ElmchemyComment
findCommentType ( regex, commentType ) acc =
    case acc of
        Normal content ->
            if Regex.contains regex content then
                commentType <|
                    Regex.replace (Regex.AtMost 1) regex (\_ -> "") content
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
            [ "{" ++ name ++ ", 0}" ]

        _ ->
            Debug.crash ("You can't export " ++ toString exp)
