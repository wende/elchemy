module Elchemy.Expression exposing (elixirE)

import Ast.Expression exposing (Expression(..))
import Elchemy.Context as Context
    exposing
        ( Context
        , areMatchingArity
        , outdent
        , inArgs
        , indent
        , mergeVariables
        , onlyWithoutFlag
        )
import Elchemy.Elixir as Elixir exposing (EAst(..))
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
        , toSnakeCaseAtom
        , translateOperator
        )
import Elchemy.Operator as Operator
import Elchemy.Selector as Selector
import Elchemy.Type as Type
import Elchemy.Variable as Variable exposing (rememberVariables)


{-| Encode any given expression
-}
elixirE : Context -> Expression -> String
elixirE c e =
    case e of
        Variable var ->
            Elixir.cheat <| elixirVariable c var

        -- Primitive types
        (Application name arg) as application ->
            tupleOrFunction c application

        RecordUpdate name keyValuePairs ->
            Elixir.cheat <|
                EMapUpdate
                    (EVar name)
                    (List.map (\( a, b ) -> ( EVar <| toSnakeCase a, ECheated (elixirE c b) )) keyValuePairs)

        -- Primitive operators
        Access (Variable modules) right ->
            modulePath modules
                ++ "."
                ++ String.join "." (List.map toSnakeCase right)

        Access left [] ->
            Elixir.cheat (ECheated <| elixirE c left)

        Access left (right :: rest) ->
            Elixir.cheat <| EAccess (ECheated <| elixirE c left) (ECheated <| elixirE c (Access (Variable [ right ]) rest))

        AccessFunction name ->
            Elixir.cheat <|
                ELambda [ EVar "x1" ] (EAccess (EVar "x1") (EVar <| toSnakeCase name))

        BinOp (Variable [ op ]) l r ->
            Elixir.cheat <| Operator.elixirBinop c elixirE op l r

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
                |> List.map (Tuple.first >> Variable.extractName c >> toSnakeCase)

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
                    |> List.map (\( name, body ) -> ind (c.indent + 1) ++ toSnakeCase name ++ ": " ++ body)
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
                    => toSnakeCase name
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
                        => toSnakeCase name
                        ++ " = rec "
                        ++ toSnakeCase name
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
                ++ (List.map (\( a, b ) -> toSnakeCase a ++ ": " ++ elixirE c b) keyValuePairs
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
flattenApplication : Expression -> List Expression
flattenApplication application =
    case application of
        Application left right ->
            if isMacro application || isTuple application then
                flattenApplication left ++ [ right ]
            else
                [ application ]

        other ->
            [ other ]


{-| Returns uncurried function application if arguments length is matching definition arity
otherwise returns curried version
-}
functionApplication : Context -> Expression -> Expression -> EAst
functionApplication c left right =
    let
        reduceArgs c args =
            List.map (ECheated << elixirE c) args

        reduceArgs2 c args separator =
            args |> List.map (elixirE c) |> String.join separator
    in
        case applicationToList (Application left right) of
            ((Variable [ fn ]) as function) :: args ->
                -- Check if we're calling the function with all of its arguments, if yes then we don't need to curry it
                if areMatchingArity c c.mod fn args then
                    EApp (EVar <| toSnakeCase fn) (reduceArgs c args)
                else if c.inMeta then
                    Context.crash c "You need to use full "
                else
                    List.foldl
                        (\a acc -> ELambdaApp acc [ ECheated <| elixirE c a ])
                        (ECheated <| elixirE c function)
                        args

            -- Function in different module
            (Access (Variable modules) [ fn ]) :: args ->
                let
                    mod =
                        modulePath modules

                    fnName =
                        toSnakeCase fn
                in
                    -- Check if we're calling the function with all of its arguments, if yes then we don't need to curry it
                    if areMatchingArity c mod fn args then
                        EApp (EAccess (EModule mod) (EVar fnName)) (reduceArgs c args)
                    else
                        List.foldl
                            (\a acc -> ELambdaApp acc [ ECheated <| elixirE c a ])
                            (EApp (EAccess (EModule mod) (EVar fnName)) [])
                            args

            _ ->
                ELambdaApp (ECheated <| elixirE c left) [ ECheated <| elixirE c right ]


encodeAccessMacroAndRest : Context -> ( Selector.AccessMacro, List Expression ) -> String
encodeAccessMacroAndRest c ( Selector.AccessMacro t arity selectors, rest ) =
    let
        encodeSelector (Selector.Access s) =
            ":" ++ toSnakeCase s

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
    case flattenApplication a of
        -- Not a macro
        (Application left right) :: [] ->
            Elixir.cheat <| functionApplication c left right

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
                                aliasFor (Context.changeCurrentModule (Context.maybeModuleAlias c mod) c) last rest
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
elixirVariable : Context -> List String -> Elixir.EAst
elixirVariable c var =
    case var of
        [] ->
            EVar ""

        [ "()" ] ->
            EVar "{}"

        [ "Nothing" ] ->
            EAtom "nil"

        [ "Just" ] ->
            ELambda [ EVar "x1" ] (ETuple [ EVar "x1" ])

        [ "Err" ] ->
            ELambda [ EVar "x1" ] (ETuple [ EAtom "error", EVar "x1" ])

        [ "Ok" ] ->
            ELambda [ EVar "x1" ] (ETuple [ EAtom "ok", EVar "x1" ])

        [ "curry" ] ->
            EApp (EVar "curried") []

        [ "uncurry" ] ->
            EApp (EVar "uncurried") []

        list ->
            -- TODO move this out of this module
            Helpers.moduleAccess c.mod list
                |> (\( mod, name ) ->
                        if isCapitilzed name then
                            aliasFor (Context.changeCurrentModule mod c) name []
                                |> Maybe.map ECheated
                                |> Maybe.withDefault (EAtom <| toSnakeCaseAtom name)
                        else if String.startsWith "@" name then
                            String.dropLeft 1 name
                                |> toSnakeCaseAtom
                                |> EAtom
                        else
                            case operatorType name of
                                Builtin ->
                                    -- We need a curried version, so kernel won't work
                                    if name == "<|" then
                                        ELambdaApp (ELambdaApp (EVar "flip") [ ELambdify 0 (EVar "|>") ]) []
                                    else
                                        ELambdaApp
                                            (ELambdify 0
                                                (EAccess
                                                    (EModule "XBasics")
                                                    (EVar <| translateOperator name)
                                                )
                                            )
                                            []

                                Custom ->
                                    translateOperator name |> EVar

                                None ->
                                    name |> toSnakeCase |> Variable.varOrNah c
                   )
