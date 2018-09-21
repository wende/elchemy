module Elchemy.Statement exposing (elixirS, moduleStatement)

import Ast
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Statement(..), Type(..))
import Dict exposing (Dict)
import Elchemy.Alias as Alias
import Elchemy.Ast as Ast
import Elchemy.Context as Context exposing (Context, Definition, deindent, indent, onlyWithoutFlag)
import Elchemy.Expression as Expression
import Elchemy.Ffi as Ffi
import Elchemy.Function as Function
import Elchemy.Type as Type
import Elchemy.Helpers as Helpers
    exposing
        ( (=>)
        , Operator(..)
        , filterMaybe
        , ind
        , indAll
        , indNoNewline
        , isCustomOperator
        , modulePath
        , operatorType
        , prependAll
        , toSnakeCase
        , translateOperator
        , typeApplicationToList
        )
import Regex exposing (HowMany(..), Regex, regex)


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
            Context.empty (modulePath path) exports

        other ->
            Debug.crash "First statement must be module declaration"


typeDefinition : Context -> String -> List Type -> List Type -> Bool -> ( Context, String )
typeDefinition c name args types isUnion =
    let
        ( newC, code ) =
            c.lastDoc
                |> Maybe.map (elixirDoc c Typedoc name)
                |> Maybe.withDefault ( c, "" )

        getVariableName t =
            case t of
                TypeVariable name ->
                    name

                _ ->
                    Context.crash c (toString t ++ " is not a type variable")

        arguments =
            if args == [] then
                ""
            else
                "("
                    ++ (List.map getVariableName args |> String.join ", ")
                    ++ ")"

        mapType =
            (if isUnion then
                Type.uniontype { c | inTypeDefiniton = True }
             else
                Type.elixirT False { c | inTypeDefiniton = True }
            )
                << Alias.replaceTypeAliases c
    in
        (,) newC <|
            onlyWithoutFlag c "notype" name <|
                code
                    ++ ind c.indent
                    ++ "@type "
                    ++ toSnakeCase True name
                    ++ arguments
                    ++ " :: "
                    ++ (List.map mapType types |> String.join " | ")
                    ++ "\n"


{-| Encode any statement
-}
elixirS : Context -> Statement -> ( Context, String )
elixirS c s =
    case s of
        InfixDeclaration _ _ _ ->
            ( c, "" )

        TypeDeclaration (TypeConstructor [ name ] args) types ->
            typeDefinition c name args types True

        TypeAliasDeclaration (TypeConstructor [ name ] args) t ->
            typeDefinition c name args [ t ] False

        FunctionTypeDeclaration "meta" t ->
            if t == TypeConstructor [ "List" ] [ TypeConstructor [ "Macro" ] [] ] then
                ( c, "" )
            else
                Context.crash c "Function `meta` is reserved and its type has to be of List Macro"

        FunctionDeclaration "meta" [] body ->
            if not <| definitionExists "meta" c then
                Context.crash c "Function `meta` requires type definition of List Macro"
            else
                ( { c | meta = Just body }, "" )

        FunctionTypeDeclaration name typedef ->
            ( c, "" )

        FunctionDeclaration name args body ->
            let
                definition =
                    c.commons.modules
                        |> Dict.get c.mod
                        |> Maybe.andThen (.definitions >> Dict.get name >> Maybe.map .def)
                        |> Maybe.map (Alias.replaceTypeAliases c)

                ( newC, code ) =
                    c.lastDoc
                        |> Maybe.map (elixirDoc c Fundoc name)
                        |> Maybe.withDefault ( c, "" )

                spec =
                    onlyWithoutFlag newC "nodef" name code
                        ++ (case operatorType name of
                                Builtin ->
                                    -- TODO implement operator specs
                                    ""

                                Custom ->
                                    definition
                                        |> Maybe.map
                                            (\def ->
                                                onlyWithoutFlag newC "nospec" name <|
                                                    ind newC.indent
                                                        ++ "@spec "
                                                        ++ translateOperator name
                                                        ++ Type.typespec newC def
                                            )
                                        |> Maybe.withDefault ""

                                None ->
                                    definition
                                        |> Maybe.map
                                            (\def ->
                                                onlyWithoutFlag newC "nospec" name <|
                                                    ind newC.indent
                                                        ++ "@spec "
                                                        ++ toSnakeCase True name
                                                        ++ Type.typespec newC def
                                            )
                                        |> Maybe.withDefault ""
                           )

                genFfi =
                    Ffi.generateFfi c Expression.elixirE name <|
                        (c.commons.modules
                            |> Dict.get c.mod
                            |> Maybe.andThen (.definitions >> Dict.get name)
                            |> Maybe.map (.def >> typeApplicationToList)
                            |> Maybe.withDefault []
                            |> List.map typeApplicationToList
                        )

                isPrivate =
                    Context.isPrivate c name

                isTuple t =
                    case t of
                        Tuple _ ->
                            True

                        _ ->
                            False
            in
                newC
                    => (case body of
                            (Application (Application (Variable [ "ffi" ]) _) _) as app ->
                                spec
                                    ++ ind (c.indent + 1)
                                    ++ genFfi app

                            (Application (Application (Variable [ "tryFfi" ]) _) _) as app ->
                                spec
                                    ++ ind (c.indent + 1)
                                    ++ genFfi app

                            (Application (Application (Variable [ "macro" ]) _) _) as app ->
                                ind c.indent
                                    ++ genFfi app
                                    ++ "\n"

                            -- Case ((Variable [ _ ]) as var) expressions ->
                            --     if [ var ] == args then
                            --         Function.genOverloadedFunctionDefinition c Expression.elixirE name args body expressions
                            --     else
                            --         Function.genFunctionDefinition c Expression.elixirE name args body
                            --
                            -- Case (Tuple vars) expressions ->
                            --     if vars == args && List.all (Tuple.first >> isTuple) expressions then
                            --         Function.genOverloadedFunctionDefinition c Expression.elixirE name args body expressions
                            --     else
                            --         Function.genFunctionDefinition c Expression.elixirE name args body
                            _ ->
                                spec
                                    ++ Function.genFunctionDefinition c Expression.elixirE name args body
                       )

        Comment content ->
            elixirComment c content

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            c
                => ind c.indent
                ++ "alias "
                ++ modulePath path

        ImportStatement path (Just asName) Nothing ->
            c
                => ind c.indent
                ++ "alias "
                ++ modulePath path
                ++ ", as: "
                ++ asName

        ImportStatement path Nothing (Just ((SubsetExport exports) as subset)) ->
            let
                moduleName =
                    path |> String.join "."

                imports =
                    List.map exportSetToList exports
                        |> List.foldr (++) []

                excepts =
                    c.commons.modules
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
                (c
                    |> insertImports moduleName subset
                    |> Context.mergeTypes subset (modulePath path)
                )
                    => ind c.indent
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
                    c.commons.modules
                        |> Dict.get mod
                        |> Maybe.map (.definitions >> Dict.keys)
                        |> Maybe.withDefault []

                excepts =
                    c.commons.modules
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
                (Context.mergeTypes AllExport mod c
                    |> insertImports mod AllExport
                )
                    => ind c.indent
                    ++ "import "
                    ++ ([ [ mod ], except ]
                            |> List.foldr (++) []
                            |> String.join ", "
                       )

        s ->
            (,) c <|
                Context.notImplemented c "statement" s


definitionExists : String -> Context -> Bool
definitionExists name c =
    c.commons.modules
        |> Dict.get c.mod
        |> Maybe.andThen (.definitions >> Dict.get name)
        |> (/=) Nothing


{-| Based on ExportSet of the `import` call inserts all of the imported types
and functions into the current context
-}
insertImports : String -> ExportSet -> Context -> Context
insertImports mod subset c =
    let
        exportNames =
            Type.getExportedTypeNames c mod subset

        importedFunctions subset =
            case subset of
                AllExport ->
                    c.commons.modules
                        |> Dict.get mod
                        |> Maybe.map .definitions
                        |> Maybe.map Dict.toList
                        |> Maybe.withDefault []
                        |> List.map (\( key, { arity } ) -> ( key, arity ))

                SubsetExport list ->
                    List.concatMap importedFunctions list

                FunctionExport name ->
                    c.commons.modules
                        |> Dict.get mod
                        |> Maybe.map .definitions
                        |> Maybe.andThen (Dict.get name)
                        |> Maybe.map (\{ arity } -> [ ( name, arity ) ])
                        |> Maybe.withDefault []

                TypeExport _ _ ->
                    []
    in
        { c
            | importedTypes = List.foldl (flip Dict.insert mod) c.importedTypes exportNames
            , importedFunctions =
                importedFunctions subset
                    |> List.foldl (\( f, arity ) acc -> Dict.insert f ( mod, arity ) acc) c.importedFunctions
        }


{-| Verify correct flag format
-}
verifyFlag : Context -> List String -> Maybe ( String, String )
verifyFlag c flag =
    case flag of
        [ k, v ] ->
            Just ( k, v )

        [ "" ] ->
            Nothing

        a ->
            Context.crash c <| "Wrong flag format " ++ toString a


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
                    |> List.map (Regex.replace All (regex "^   ") (always ""))
                    -- |> List.map String.trim
                    |> String.join "\n"
                    |> indAll c.indent
                )

        Flag content ->
            flip (,) "" <|
                (content
                    |> Regex.split All (regex "\\s+")
                    |> List.map (String.split ":+")
                    |> List.filterMap (verifyFlag c)
                    |> List.foldl Context.addFlag c
                )

        Normal content ->
            (,) c <|
                (content
                    |> prependAll "# "
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
            ind c.indent
                ++ prefix
                ++ " \"\"\"\n "
                ++ (content
                        |> String.lines
                        |> List.map (maybeDoctest c name)
                        |> List.map Helpers.escape
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
        [ ( "^\\sex\\b", Ex )
        , ( "^\\|", Doc )
        , ( "^\\sflag\\b", Flag )
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

        AllExport ->
            []

        SubsetExport _ ->
            []


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
                    ++ (c.commons.modules
                            |> Dict.get c.mod
                            |> Maybe.map .definitions
                            |> Maybe.andThen (Dict.get name)
                            |> Maybe.map .arity
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
                        Context.getShadowedFunctions c Helpers.reservedBasicFunctions
                            ++ Context.getShadowedFunctions c Helpers.reservedKernelFunctions
                            |> List.filter (Tuple.first >> (==) forName)

                    importBasics =
                        if shadowed == [] then
                            ""
                        else
                            Context.importBasicsWithoutShadowed c
                                |> String.trimRight
                                |> String.split "\n"
                                |> String.join (ind (c.indent + 2) ++ "iex> ")
                                |> (++) (indNoNewline 1 ++ "iex> ")
                                |> flip (++) (ind 0)
                in
                    importBasics
                        ++ indNoNewline (c.indent + 1)
                        ++ "iex> import "
                        ++ c.mod
                        ++ ind (c.indent + 2)
                        ++ "iex> "
                        ++ Expression.elixirE c l
                        ++ ind (c.indent + 2)
                        ++ Expression.elixirE c r
                        ++ "\n"

            _ ->
                line
    else
        line
