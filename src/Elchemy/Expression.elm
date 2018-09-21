module Elchemy.Expression exposing (elixirE)

import Ast.Expression exposing (Expression(..))
import Elchemy.Context as Context
    exposing
        ( Context
        , areMatchingArity
        , deindent
        , inArgs
        , indent
        , mergeVariables
        , onlyWithoutFlag
        )
import Elchemy.Operator as Operator
import Elchemy.Selector as Selector
import Elchemy.Type as Type
import Elchemy.Variable as Variable exposing (rememberVariables)
import Elchemy.Helpers as Helpers
    exposing
        ( (=>)
        , Operator(..)
        , applicationToList
        , atomize
        , generateArguments
        , ind
        , isCapitilzed
        , lastAndRest
        , maybeOr
        , modulePath
        , operatorType
        , toSnakeCase
        , translateOperator
        )


{-| Encode any given expression
-}
elixirE : Context -> Expression -> String
elixirE c e =
    case e of
        Variable var ->
            elixirVariable c var

        -- Primitive types
        (Application name arg) as application ->
            tupleOrFunction c application

        RecordUpdate name keyValuePairs ->
            "%{"
                ++ toSnakeCase True name
                ++ " | "
                ++ (List.map (\( a, b ) -> toSnakeCase True a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        -- Primitive operators
        Access (Variable modules) right ->
            modulePath modules
                ++ "."
                ++ String.join "." (List.map (toSnakeCase True) right)

        Access left right ->
            elixirE c left
                ++ "."
                ++ String.join "." right

        AccessFunction name ->
            "(fn a -> a." ++ toSnakeCase True name ++ " end)"

        BinOp (Variable [ op ]) l r ->
            Operator.elixirBinop c elixirE op l r

        -- Rest
        e ->
            elixirControlFlow c e


{-| Encode control flow expressions
-}
elixirControlFlow : Context -> Expression -> String
elixirControlFlow c e =
    case e of
        Case var body ->
            caseE c var body

        Lambda args body ->
            lambda c args body

        (If check onTrue ((If _ _ _) as onFalse)) as exp ->
            [ "cond do" ]
                ++ handleIfExp (indent c) exp
                ++ [ ind c.indent, "end" ]
                |> String.join ""

        If check onTrue onFalse ->
            "if "
                ++ elixirE c check
                ++ " do "
                ++ elixirE c onTrue
                ++ " else "
                ++ elixirE c onFalse
                ++ " end"

        Let variables expression ->
            variables
                |> Variable.organizeLetInVariablesOrder c
                |> Variable.groupByCrossDependency
                |> (flip List.foldl ( c, "" ) <|
                        \varGroup ( cAcc, codeAcc ) ->
                            (case varGroup of
                                [] ->
                                    cAcc => ""

                                [ ( var, exp ) ] ->
                                    elixirLetInBranch cAcc ( var, exp )

                                multiple ->
                                    elixirLetInMutualFunctions cAcc multiple
                            )
                                |> (\( c, string ) ->
                                        mergeVariables c cAcc
                                            => codeAcc
                                            ++ string
                                            ++ ind c.indent
                                   )
                   )
                |> (\( c, code ) -> code ++ elixirE c expression)

        _ ->
            elixirPrimitve c e


{-| Encodes a mutual function usage into `let` macro
-}
elixirLetInMutualFunctions : Context -> List ( Expression, Expression ) -> ( Context, String )
elixirLetInMutualFunctions context expressionsList =
    let
        vars =
            List.map Tuple.first expressionsList

        names =
            expressionsList
                |> List.map (Tuple.first >> Variable.extractName c >> toSnakeCase True)

        c =
            rememberVariables vars context

        letBranchToLambda : Context -> ( Expression, Expression ) -> String
        letBranchToLambda c ( head, body ) =
            case applicationToList head of
                [] ->
                    ""

                [ single ] ->
                    elixirE c body

                (Variable [ name ]) :: args ->
                    lambda c args body

                _ ->
                    Context.crash c <| toString head ++ " is not a let in branch"
    in
        c
            => "{"
            ++ (names |> String.join ", ")
            ++ "} = let ["
            ++ (expressionsList
                    |> List.map (\(( var, exp ) as v) -> ( Variable.extractName c var, v ))
                    |> List.map (Tuple.mapSecond <| letBranchToLambda (indent c))
                    |> List.map (\( name, body ) -> ind (c.indent + 1) ++ toSnakeCase True name ++ ": " ++ body)
                    |> String.join ","
               )
            ++ ind c.indent
            ++ "]"


{-| Encodes a branch of let..in expression

    let
      {a, b} == 2 --< This is a branch
    in
      10

-}
elixirLetInBranch : Context -> ( Expression, Expression ) -> ( Context, String )
elixirLetInBranch c ( left, exp ) =
    let
        wrapElixirE c exp =
            case exp of
                Let _ _ ->
                    "(" ++ ind (c.indent + 1) ++ elixirE (indent c) exp ++ ind c.indent ++ ")"

                _ ->
                    elixirE c exp
    in
        case applicationToList left of
            [ (Variable [ name ]) as var ] ->
                rememberVariables [ var ] c
                    => toSnakeCase True name
                    ++ " = "
                    ++ wrapElixirE (c |> rememberVariables [ var ]) exp

            ((Variable [ name ]) as var) :: args ->
                if Helpers.isCapitilzed name then
                    (c |> rememberVariables args)
                        => tupleOrFunction (rememberVariables args c) left
                        ++ " = "
                        ++ wrapElixirE c exp
                else
                    rememberVariables [ var ] c
                        => toSnakeCase True name
                        ++ " = rec "
                        ++ toSnakeCase True name
                        ++ ", "
                        ++ lambda (c |> rememberVariables [ var ]) args exp

            [ assign ] ->
                rememberVariables [ assign ] c
                    => elixirE (inArgs c) assign
                    ++ " = "
                    ++ wrapElixirE (rememberVariables [ assign ] c) exp

            _ ->
                c => ""


{-| Encode primitive value
-}
elixirPrimitve : Context -> Expression -> String
elixirPrimitve c e =
    case e of
        Integer value ->
            toString value

        Float value ->
            let
                name =
                    toString value
            in
                if String.contains "." name then
                    name
                else
                    name ++ ".0"

        Character value ->
            case value of
                ' ' ->
                    "?\\s"

                '\n' ->
                    "?\\n"

                '\x0D' ->
                    "?\\r"

                '\t' ->
                    "?\\t"

                '\\' ->
                    "?\\\\"

                '\x00' ->
                    "?\\0"

                other ->
                    "?" ++ String.fromChar other

        String value ->
            "\"" ++ value ++ "\""

        List vars ->
            "["
                ++ (List.map (elixirE c) vars
                        |> String.join ", "
                   )
                ++ "]"

        Tuple vars ->
            "{"
                ++ (List.map (elixirE c) vars
                        |> String.join ", "
                   )
                ++ "}"

        Record keyValuePairs ->
            "%{"
                ++ (List.map (\( a, b ) -> toSnakeCase True a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        _ ->
            Context.notImplemented c "expression" e


{-| Change if expression body into list of clauses
-}
handleIfExp : Context -> Expression -> List String
handleIfExp c e =
    case e of
        If check onTrue onFalse ->
            [ ind c.indent
            , elixirE (indent c) check
            , " -> "
            , elixirE (indent c) onTrue
            ]
                ++ handleIfExp c onFalse

        _ ->
            [ ind c.indent
            , "true -> "
            , elixirE (indent c) e
            ]


{-| Returns if called function is a special macro inline by the compiler
-}
isMacro : Expression -> Bool
isMacro e =
    case e of
        Application a _ ->
            isMacro a

        Variable [ x ] ->
            List.member x
                [ "tryFfi"
                , "ffi"
                , "lffi"
                , "macro"
                , "flambda"
                , "updateIn"
                , "updateIn2"
                , "updateIn3"
                , "updateIn4"
                , "updateIn5"
                , "putIn"
                , "putIn"
                , "putIn2"
                , "putIn3"
                , "putIn4"
                , "putIn5"
                , "getIn"
                , "getIn2"
                , "getIn3"
                , "getIn4"
                , "getIn5"
                ]

        other ->
            False


{-| Flattens Type application into a List of expressions or returns a singleton if it's not a type
-}
flattenTypeApplication : Expression -> List Expression
flattenTypeApplication application =
    case application of
        Application left right ->
            if isMacro application || isTuple application then
                flattenTypeApplication left ++ [ right ]
            else
                [ application ]

        other ->
            [ other ]


{-| Returns uncurried function application if arguments length is matching definition arity
otherwise returns curried version
-}
functionApplication : Context -> Expression -> Expression -> String
functionApplication c left right =
    let
        reduceArgs c args separator =
            args |> List.map (elixirE c) |> String.join separator
    in
        case applicationToList (Application left right) of
            (Variable [ fn ]) :: args ->
                if areMatchingArity c c.mod fn args then
                    toSnakeCase True fn ++ "(" ++ reduceArgs c args ", " ++ ")"
                else if c.inMeta then
                    Context.crash c "You need to use full "
                else
                    elixirE c left ++ ".(" ++ elixirE c right ++ ")"

            (Access (Variable modules) [ fn ]) :: args ->
                let
                    mod =
                        modulePath modules

                    fnName =
                        toSnakeCase True fn
                in
                    if areMatchingArity c mod fn args then
                        mod ++ "." ++ fnName ++ "(" ++ reduceArgs c args ", " ++ ")"
                    else
                        mod ++ "." ++ fnName ++ "().(" ++ reduceArgs c args ").(" ++ ")"

            _ ->
                elixirE c left ++ ".(" ++ elixirE c right ++ ")"


encodeAccessMacroAndRest : Context -> ( Selector.AccessMacro, List Expression ) -> String
encodeAccessMacroAndRest c ( Selector.AccessMacro t arity selectors, rest ) =
    let
        encodeSelector (Selector.Access s) =
            ":" ++ toSnakeCase True s

        encodedSelectors =
            selectors |> List.map encodeSelector |> String.join ", "

        encodedType =
            case t of
                Selector.Update ->
                    "update_in_"

                Selector.Get ->
                    "get_in_"

                Selector.Put ->
                    "put_in_"

        encodedRest =
            case rest of
                [] ->
                    ""

                list ->
                    ".("
                        ++ (List.map (elixirE c) rest |> String.join ").(")
                        ++ ")"
    in
        encodedType
            ++ "(["
            ++ encodedSelectors
            ++ "])"
            ++ encodedRest


{-| Returns code representation of tuple or function depending on definition
-}
tupleOrFunction : Context -> Expression -> String
tupleOrFunction c a =
    case flattenTypeApplication a of
        -- Not a macro
        (Application left right) :: [] ->
            functionApplication c left right

        -- A macro
        (Variable [ "ffi" ]) :: rest ->
            Context.crash c "Ffi inside function body is deprecated since Elchemy 0.3"

        (Variable [ "macro" ]) :: rest ->
            Context.crash c "You can't use `macro` inside a function body"

        (Variable [ "tryFfi" ]) :: rest ->
            Context.crash c "tryFfi inside function body is deprecated since Elchemy 0.3"

        (Variable [ "lffi" ]) :: rest ->
            Context.crash c "Lffi inside function body is deprecated since Elchemy 0.3"

        (Variable [ "flambda" ]) :: rest ->
            Context.crash c "Flambda is deprecated since Elchemy 0.3"

        [ Variable [ "Just" ], arg ] ->
            "{" ++ elixirE c arg ++ "}"

        [ Variable [ "Ok" ], arg ] ->
            "{:ok, " ++ elixirE c arg ++ "}"

        [ Variable [ "Err" ], arg ] ->
            "{:error, " ++ elixirE c arg ++ "}"

        [ Variable [ "Do" ], arg ] ->
            "quote do " ++ elixirE c arg ++ " end"

        -- Regular non-macro application
        ((Variable list) as call) :: rest ->
            Selector.maybeAccessMacro c call rest
                |> Maybe.map (encodeAccessMacroAndRest c)
                |> Maybe.withDefault
                    (Helpers.moduleAccess c.mod list
                        |> (\( mod, last ) ->
                                aliasFor (Context.changeCurrentModule mod c) last rest
                                    |> Maybe.withDefault
                                        ("{"
                                            ++ elixirE c (Variable [ last ])
                                            ++ ", "
                                            ++ (List.map (elixirE c) rest |> String.join ", ")
                                            ++ "}"
                                        )
                           )
                    )

        other ->
            Context.crash c ("Shouldn't ever work for" ++ toString other)


{-| Return an alias for type alias or union type if it exists, return Nothing otherwise
-}
aliasFor : Context -> String -> List Expression -> Maybe String
aliasFor c name rest =
    maybeOr (typeAliasApplication c name rest) (typeApplication c name rest)


{-| Returns Just only if the passed alias type is a type alias
-}
filterTypeAlias : Context.Alias -> Maybe Context.Alias
filterTypeAlias ({ aliasType } as ali) =
    case aliasType of
        Context.TypeAlias ->
            Just ali

        Context.Type ->
            Nothing


{-| Returns a type alias application based on current context definitions
-}
typeAliasApplication : Context -> String -> List Expression -> Maybe String
typeAliasApplication c name args =
    Context.getAlias c.mod name c
        |> Maybe.andThen filterTypeAlias
        |> Maybe.andThen (Type.typeAliasConstructor args)
        |> Maybe.map (elixirE c)


{-| Returns a type application based on current context definitions
-}
typeApplication : Context -> String -> List Expression -> Maybe String
typeApplication c name args =
    Context.getType c.mod name c
        |> (Maybe.map <|
                \{ arity } ->
                    let
                        len =
                            List.length args

                        dif =
                            arity - len

                        arguments =
                            generateArguments dif

                        varArgs =
                            List.map (List.singleton >> Variable) arguments
                    in
                        if arity == 0 then
                            atomize name
                        else if dif >= 0 then
                            (arguments
                                |> List.map ((++) "fn ")
                                |> List.map (flip (++) " -> ")
                                |> String.join ""
                            )
                                ++ "{"
                                ++ atomize name
                                ++ ", "
                                ++ (List.map (rememberVariables varArgs c |> elixirE) (args ++ varArgs)
                                        |> String.join ", "
                                   )
                                ++ "}"
                                |> flip (++) (String.repeat dif " end")
                        else
                            Context.crash c <|
                                "Expected "
                                    ++ toString arity
                                    ++ " arguments for '"
                                    ++ name
                                    ++ "'. Got: "
                                    ++ toString (List.length args)
           )


{-| Returns True if an expression is type application or false if it's a regular application
-}
isTuple : Expression -> Bool
isTuple a =
    case a of
        Application a _ ->
            isTuple a

        Variable [ "()" ] ->
            True

        Variable [ name ] ->
            isCapitilzed name

        Variable list ->
            Helpers.moduleAccess "" list
                |> (\( _, last ) -> isTuple (Variable [ last ]))

        other ->
            False


{-| Create 'case' expression by passing a value being "cased on" and list of branches
-}
caseE : Context -> Expression -> List ( Expression, Expression ) -> String
caseE c var body =
    "case "
        ++ elixirE c var
        ++ " do"
        ++ String.join "" (List.map (rememberVariables [ var ] c |> caseBranch) body)
        ++ ind c.indent
        ++ "end"


{-| Create a single branch of case statement by giving left and right side of the arrow
-}
caseBranch : Context -> ( Expression, Expression ) -> String
caseBranch c ( left, right ) =
    (ind (c.indent + 1) ++ elixirE (inArgs c) left)
        ++ " ->"
        ++ ind (c.indent + 2)
        ++ elixirE (c |> indent |> indent |> rememberVariables [ left ]) right


{-| Used to encode a function and create a curried function from a lambda expression
-}
lambda : Context -> List Expression -> Expression -> String
lambda c args body =
    case args of
        arg :: rest ->
            "fn "
                ++ elixirE (inArgs c) arg
                ++ " -> "
                ++ lambda (c |> rememberVariables [ arg ]) rest body
                ++ " end"

        [] ->
            elixirE c body


{-| Produce a variable out of it's expression, considering some of the hardcoded values
used for easier interaction with Elixir
-}
elixirVariable : Context -> List String -> String
elixirVariable c var =
    case var of
        [] ->
            ""

        [ "()" ] ->
            "{}"

        [ "Nothing" ] ->
            "nil"

        [ "Just" ] ->
            "fn x1 -> {x1} end"

        [ "Err" ] ->
            "fn x1 -> {:error, x1} end"

        [ "Ok" ] ->
            "fn x1 -> {:ok, x1} end"

        [ "curry" ] ->
            "curried()"

        [ "uncurry" ] ->
            "uncurried()"

        list ->
            Helpers.moduleAccess c.mod list
                |> (\( mod, name ) ->
                        if isCapitilzed name then
                            aliasFor (Context.changeCurrentModule mod c) name []
                                |> Maybe.withDefault (atomize name)
                        else if String.startsWith "@" name then
                            String.dropLeft 1 name
                                |> atomize
                        else
                            case operatorType name of
                                Builtin ->
                                    -- We need a curried version, so kernel won't work
                                    if name == "<|" then
                                        "flip().((&|>/0).())"
                                    else
                                        "(&XBasics." ++ translateOperator name ++ "/0).()"

                                Custom ->
                                    translateOperator name

                                None ->
                                    name |> toSnakeCase True |> Variable.varOrNah c
                   )
