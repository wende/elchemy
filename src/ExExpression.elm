module ExExpression exposing (elixirE)

import ExType
import ExOperator
import ExVariable exposing (rememberVariables)
import Ast.Expression exposing (Expression(..))
import Helpers
    exposing
        ( toSnakeCase
        , modulePath
        , ind
        , applicationToList
        , (=>)
        , notImplemented
        , lastAndRest
        , maybeOr
        , generateArguments
        , atomize
        , isCapitilzed
        , operatorType
        , Operator(..)
        , translateOperator
        )
import ExContext
    exposing
        ( Context
        , indent
        , deindent
        , onlyWithoutFlag
        , inArgs
        , mergeVariables
        , getArity
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
            (tupleOrFunction c application)

        RecordUpdate name keyValuePairs ->
            "%{"
                ++ toSnakeCase True name
                ++ " | "
                ++ (List.map (\( a, b ) -> a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        -- Primitive operators
        Access ((Variable modules) as left) right ->
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
            ExOperator.elixirBinop c elixirE op l r

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
                |> (flip List.foldl ( c, "" ) <|
                        \( var, exp ) ( cAcc, codeAcc ) ->
                            (case applicationToList var of
                                [ (Variable [ name ]) as var ] ->
                                    rememberVariables [ var ] cAcc
                                        => toSnakeCase True name
                                        ++ " = "
                                        ++ elixirE (cAcc |> rememberVariables [ var ]) exp

                                ((Variable [ name ]) as var) :: args ->
                                    rememberVariables [ var ] cAcc
                                        => toSnakeCase True name
                                        ++ " = rec "
                                        ++ toSnakeCase True name
                                        ++ ", "
                                        ++ lambda (cAcc |> rememberVariables [ var ]) args exp

                                [ assign ] ->
                                    rememberVariables [ assign ] cAcc
                                        => elixirE (inArgs cAcc) assign
                                        ++ " = "
                                        ++ elixirE cAcc exp

                                _ ->
                                    Debug.crash "Impossible"
                            )
                                |> (\( c, string ) ->
                                        mergeVariables c cAcc
                                            => codeAcc
                                            ++ string
                                            ++ (ind c.indent)
                                   )
                   )
                |> (\( c, code ) -> code ++ (elixirE c expression))

        _ ->
            elixirPrimitve c e


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
                ++ (List.map (\( a, b ) -> a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        _ ->
            notImplemented "expression" e


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

        Variable [ "tryFfi" ] ->
            True

        Variable [ "ffi" ] ->
            True

        Variable [ "lffi" ] ->
            True

        Variable [ "flambda" ] ->
            True

        other ->
            False


{-| Flattens Type application into a List of expressions or returns a singleton if it's not a type
-}
flattenTypeApplication : Expression -> List Expression
flattenTypeApplication application =
    case application of
        Application left right ->
            if isMacro application then
                (flattenTypeApplication left) ++ [ right ]
            else if isTuple application then
                (flattenTypeApplication left) ++ [ right ]
            else
                [ application ]

        other ->
            [ other ]


{-| Returns uncurryied function application if arguments length is matching definition arity
otherwise returns curried version
-}
functionApplication : Context -> Expression -> Expression -> String
functionApplication c left right =
    case applicationToList (Application left right) of
        (Variable [ fn ]) :: args ->
            if List.length args == Maybe.withDefault -1 (getArity c.mod fn c) then
                fn
                    ++ "("
                    ++ (args |> List.map (elixirE c) |> String.join ", ")
                    ++ ")"
            else
                elixirE c left ++ ".(" ++ elixirE c right ++ ")"

        _ ->
            elixirE c left ++ ".(" ++ elixirE c right ++ ")"


{-| Returns code representation of tuple or function depending on definition
-}
tupleOrFunction : Context -> Expression -> String
tupleOrFunction c a =
    case flattenTypeApplication a of
        (Application left right) :: [] ->
            functionApplication c left right

        (Variable [ "ffi" ]) :: rest ->
            Debug.crash "Ffi inside function body is deprecated since Elchemy 0.3"

        (Variable [ "tryFfi" ]) :: rest ->
            Debug.crash "tryFfi inside function body is deprecated since Elchemy 0.3"

        (Variable [ "lffi" ]) :: rest ->
            Debug.crash "Lffi inside function body is deprecated since Elchemy 0.3"

        (Variable [ "flambda" ]) :: rest ->
            Debug.crash "Flambda is deprecated since Elchemy 0.3"

        [ Variable [ "Just" ], arg ] ->
            "{" ++ elixirE c arg ++ "}"

        [ Variable [ "Ok" ], arg ] ->
            if arg == Variable [ "()" ] then
                ":ok"
            else
                "{:ok, " ++ elixirE c arg ++ "}"

        [ Variable [ "Err" ], arg ] ->
            "{:error, " ++ elixirE c arg ++ "}"

        (Variable list) :: rest ->
            case lastAndRest list of
                ( Just last, _ ) ->
                    aliasFor c last rest
                        |> (Maybe.withDefault <|
                                "{"
                                    ++ elixirE c (Variable [ last ])
                                    ++ ", "
                                    ++ (List.map (elixirE c) rest |> String.join ", ")
                                    ++ "}"
                           )

                _ ->
                    Debug.crash "Won't ever happen"

        other ->
            Debug.crash ("Shouldn't ever work for" ++ toString other)


{-| Return an alias for type alias or union type if it exists, return Nothing otherwise
-}
aliasFor : Context -> String -> List Expression -> Maybe String
aliasFor c name rest =
    maybeOr (typeAliasApplication c name rest) (typeApplication c name rest)


{-| Returns Just only if the passed alias type is a type alias
-}
filterTypeAlias : ExContext.Alias -> Maybe ExContext.Alias
filterTypeAlias ({ aliasType } as ali) =
    case aliasType of
        ExContext.TypeAlias ->
            Just ali

        ExContext.Type ->
            Nothing


{-| Returns a type alias application based on current context definitions
-}
typeAliasApplication : Context -> String -> List Expression -> Maybe String
typeAliasApplication c name args =
    ExContext.getAlias c.mod name c
        |> Maybe.andThen filterTypeAlias
        |> Maybe.andThen (ExType.typeAliasConstructor args)
        |> Maybe.map (elixirE c)


{-| Returns a type application based on current context definitions
-}
typeApplication : Context -> String -> List Expression -> Maybe String
typeApplication c name args =
    ExContext.getType c.mod name c
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
                                |> List.map ((++) " fn ")
                                |> List.map (flip (++) " ->")
                                |> String.join ""
                            )
                                ++ " {"
                                ++ atomize name
                                ++ ", "
                                ++ (List.map (rememberVariables (args ++ varArgs) c |> elixirE) (args ++ varArgs)
                                        |> String.join ", "
                                   )
                                ++ "}"
                                |> flip (++) (String.repeat dif " end ")
                        else
                            Debug.crash <|
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
            case lastAndRest list of
                ( Just last, _ ) ->
                    isTuple (Variable [ last ])

                _ ->
                    Debug.crash "Shouldn't ever happen"

        other ->
            False


{-| Create 'case' expression by passing a value being "cased on" and list of branches
-}
caseE : Context -> Expression -> List ( Expression, Expression ) -> String
caseE c var body =
    "case "
        ++ elixirE c var
        ++ " do"
        ++ (String.join "" (List.map (c |> rememberVariables [ var ] |> caseBranch) body))
        ++ ind (c.indent)
        ++ "end"


{-| Create a single branch of case statement by giving left and right side of the arrow
-}
caseBranch : Context -> ( Expression, Expression ) -> String
caseBranch c ( left, right ) =
    (ind (c.indent + 1) ++ elixirE (inArgs c) left)
        ++ " ->"
        ++ (ind (c.indent + 2))
        ++ (elixirE (c |> indent |> indent |> rememberVariables [ left ]) right)


{-| Used to create a curried function from a lambda expression
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

        [ name ] ->
            if isCapitilzed name then
                aliasFor c name []
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
                            "(&" ++ translateOperator name ++ "/0).()"

                    Custom ->
                        translateOperator name

                    None ->
                        name |> toSnakeCase True |> ExVariable.varOrNah c

        list ->
            case lastAndRest list of
                ( Just last, rest ) ->
                    elixirE c (Variable [ last ])

                _ ->
                    Debug.crash <|
                        "Shouldn't ever happen "
                            ++ String.join "." list
