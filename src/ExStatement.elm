module ExStatement exposing (elixirS, moduleStatement)

import Ast
import ExFfi
import ExType
import ExFunction
import ExExpression
import Dict exposing (Dict)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (Expression(..))
import Regex exposing (Regex, HowMany(..), regex)
import Ast.Statement exposing (Statement(..), Type(..), ExportSet(..))
import ExContext exposing (Context, Definition, indent, deindent, onlyWithoutFlag)
import Helpers
    exposing
        ( ind
        , indAll
        , indNoNewline
        , prependAll
        , toSnakeCase
        , operatorType
        , isCustomOperator
        , Operator(..)
        , translateOperator
        , (=>)
        , modulePath
        , notImplemented
        , typeApplicationToList
        , filterMaybe
        )


type ElchemyComment
    = Doc String
    | Ex String
    | Normal String
    | Flag String


type DocType
    = Fundoc
    | Typedoc
    | ModuleDoc


{-| Make sure first statement is a module declaration
-}
moduleStatement : Statement -> Context
moduleStatement s =
    case s of
        ModuleDeclaration path exports ->
            ExContext.empty (modulePath path) exports

        other ->
            Debug.crash "First statement must be module declaration"


{-| Encode any statement
-}
elixirS : Context -> Statement -> ( Context, String )
elixirS c s =
    case s of
        InfixDeclaration _ _ _ ->
            ( c, "" )

        TypeDeclaration (TypeConstructor [ name ] _) types ->
            let
                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Typedoc name)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) newC <|
                    code
                        ++ (ind c.indent)
                        ++ "@type "
                        ++ toSnakeCase True name
                        ++ " :: "
                        ++ (List.map (ExType.uniontype c) types |> String.join " | ")
                        ++ "\n"

        TypeAliasDeclaration _ _ ->
            ( c, "" )

        (FunctionTypeDeclaration name ((TypeApplication _ _) as t)) as def ->
            let
                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc name)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) newC <|
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
                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc name)
                        |> Maybe.withDefault ( c, "" )
            in
                (,) newC <|
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
                        (c.modules
                            |> Dict.get c.mod
                            |> Maybe.andThen (.definitions >> Dict.get name)
                            |> Maybe.map (.def >> typeApplicationToList)
                            |> Maybe.withDefault []
                            |> List.map typeApplicationToList
                        )

                definitionExists =
                    c.modules
                        |> Dict.get c.mod
                        |> Maybe.andThen (.definitions >> Dict.get name)
                        |> (==) Nothing
                        |> (&&) (not (ExContext.isPrivate c name))
            in
                c
                    => if definitionExists then
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

                            Case (Tuple vars) expressions ->
                                if vars == args then
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
            let
                imports =
                    List.map exportSetToList exports
                        |> List.foldr (++) []

                excepts =
                    c.modules
                        |> Dict.get c.mod
                        |> Maybe.map (.definitions >> Dict.keys >> duplicates imports)
                        |> Maybe.withDefault []

                only =
                    if imports == [] then
                        []
                    else
                        [ "only: ["
                            ++ String.join ", " (elixirExportList c imports)
                            ++ "]"
                        ]

                except =
                    if excepts == [] then
                        []
                    else
                        [ "except: ["
                            ++ String.join ", " (elixirExportList c excepts)
                            ++ "]"
                        ]

                importOrAlias =
                    if imports == [] && excepts == [] then
                        "alias "
                    else
                        "import "
            in
                ExContext.mergeTypes subset (modulePath path) c
                    => (ind c.indent)
                    ++ importOrAlias
                    ++ ([ [ modulePath path ], only, except ]
                            |> List.foldr (++) []
                            |> String.join ", "
                       )

        -- Suppresses the compiler warning
        ImportStatement [ "Elchemy" ] Nothing (Just AllExport) ->
            ( c, "" )

        ImportStatement modPath Nothing (Just AllExport) ->
            let
                mod =
                    modulePath modPath

                exports =
                    c.modules
                        |> Dict.get mod
                        |> Maybe.map (.definitions >> Dict.keys)
                        |> Maybe.withDefault []

                excepts =
                    c.modules
                        |> Dict.get c.mod
                        |> Maybe.map (.definitions >> Dict.keys >> duplicates exports)
                        |> Maybe.withDefault []

                except =
                    if excepts == [] then
                        []
                    else
                        [ "except: ["
                            ++ String.join ", " (elixirExportList c excepts)
                            ++ "]"
                        ]
            in
                ExContext.mergeTypes AllExport mod c
                    => (ind c.indent)
                    ++ "import "
                    ++ ([ [ mod ], except ]
                            |> List.foldr (++) []
                            |> String.join ", "
                       )

        s ->
            (,) c <|
                notImplemented "statement" s


{-| Verify correct flag format
-}
verifyFlag : List String -> Maybe ( String, String )
verifyFlag flag =
    case flag of
        [ k, v ] ->
            Just ( k, v )

        [ "" ] ->
            Nothing

        a ->
            Debug.crash <| "Wrong flag format " ++ toString a


{-| Encode elixir comment and return a context with updated last doc
-}
elixirComment : Context -> String -> ( Context, String )
elixirComment c content =
    case getCommentType content of
        Doc content ->
            if c.hasModuleDoc then
                { c | lastDoc = Just content } => ""
            else
                elixirDoc c ModuleDoc c.mod content

        Ex content ->
            (,) c <|
                (content
                    |> String.split "\n"
                    |> List.map String.trim
                    |> String.join "\n"
                    |> indAll c.indent
                )

        Flag content ->
            flip (,) "" <|
                (content
                    |> Regex.split All (regex "\\s+")
                    |> List.map (String.split ":+")
                    |> List.filterMap verifyFlag
                    |> List.foldl (ExContext.addFlag) c
                )

        Normal content ->
            (,) c <|
                (content
                    |> prependAll ("# ")
                    |> indAll c.indent
                )


{-| Enocode a doc and return new context
-}
elixirDoc : Context -> DocType -> String -> String -> ( Context, String )
elixirDoc c doctype name content =
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
                        |> List.map (maybeDoctest c name)
                        |> List.map (Helpers.escape)
                        |> List.map (Regex.replace All (regex "\"\"\"") (always "\\\"\\\"\\\""))
                        -- |> map trimIndentations
                        |> String.join (ind c.indent)
                        -- Drop an unnecessary ammounts of \n's
                        |> Regex.replace All (regex "\n(\n| ){3,}\n") (always "\n\n")
                   )
                ++ ind c.indent
                ++ "\"\"\""


{-| Get a type of the comment by it's content
-}
getCommentType : String -> ElchemyComment
getCommentType comment =
    let
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
    in
        [ ( "^\\sex\\b", (Ex) )
        , ( "^\\|", (Doc) )
        , ( "^\\sflag\\b", (Flag) )
        ]
            |> List.map (\( a, b ) -> ( Regex.regex a, b ))
            |> List.foldl (uncurry findCommentType) (Normal comment)


{-| Encode all exports from a module
-}
exportSetToList : ExportSet -> List String
exportSetToList exp =
    case exp of
        TypeExport _ _ ->
            []

        FunctionExport name ->
            [ name ]

        _ ->
            Debug.crash ("You can't export " ++ toString exp)


elixirExportList : Context -> List String -> List String
elixirExportList c list =
    let
        defineFor name arity =
            "{:'"
                ++ name
                ++ "', "
                ++ toString arity
                ++ "}"

        wrap name =
            if isCustomOperator name then
                defineFor (translateOperator name) 0
                    ++ ", "
                    ++ defineFor (translateOperator name) 2
            else if name == "ffi" then
                ""
            else
                defineFor (toSnakeCase True name) 0
                    ++ (c.modules
                            |> Dict.get c.mod
                            |> Maybe.map .definitions
                            |> Maybe.andThen (Dict.get name)
                            |> Maybe.map (.arity)
                            |> filterMaybe ((/=) 0)
                            |> Maybe.map (defineFor (toSnakeCase True name))
                            |> Maybe.map ((++) ", ")
                            |> Maybe.withDefault ""
                       )
    in
        List.map wrap list


duplicates : List a -> List a -> List a
duplicates listA listB =
    List.filter (flip List.member listB) listA


{-| Replace a function doc with a doctest if in correct format
-}
maybeDoctest : Context -> String -> String -> String
maybeDoctest c forName line =
    if String.startsWith (ind (c.indent + 1)) ("\n" ++ line) then
        case Ast.parseExpression Ast.BinOp.operators (String.trim line) of
            Ok ( _, _, BinOp (Variable [ "==" ]) l r ) ->
                let
                    shadowed =
                        ExContext.getShadowedStdFunctions c
                            |> List.filter (Tuple.first >> (==) forName)

                    importBasics =
                        if shadowed == [] then
                            ""
                        else
                            indNoNewline (c.indent + 1)
                                ++ "iex> "
                                ++ ExContext.importBasicsWithoutShadowed c
                                ++ indNoNewline 0
                in
                    importBasics
                        ++ indNoNewline (c.indent + 1)
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
