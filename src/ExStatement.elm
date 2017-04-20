module ExStatement exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import ExContext exposing (Context)
import ExExpression
import Helpers exposing (..)
import List exposing (..)


moduleStatement : Statement -> Context
moduleStatement s =
    case s of
        ModuleDeclaration names exports ->
            Context (String.join "." names) exports 0

        other ->
            Debug.crash "First statement must be module declaration"


elixirS : Statement -> Context -> String
elixirS s c =
    case s of
        TypeDeclaration name types ->
            ""

        -- (ind c.indent)
        --     ++ "@type "
        --     ++ (ExType.typealias name)
        --     ++ " :: "
        --     ++ ((map (ExType.typealias) types) |> String.join " | ")
        TypeAliasDeclaration _ _ ->
            ""

        --"alias?"
        FunctionTypeDeclaration name t ->
            ""

        -- (ind c.indent) ++ "@spec " ++ toSnakeCase name ++ (typespec t)
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
            (ind c.indent) ++ "#" ++ content

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            (ind c.indent) ++ "alias " ++ String.join "." path

        ImportStatement path Nothing (Just AllExport) ->
            (ind c.indent) ++ "import " ++ String.join "." path

        s ->
            notImplemented "statement" s
