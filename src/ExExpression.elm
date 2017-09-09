module ExExpression exposing (..)

import Ast.Expression exposing (..)
import Helpers exposing (..)
import ExContext
    exposing
        ( Context
        , indent
        , deindent
        , onlyWithoutFlag
        , inArgs
        , mergeVariables
        )
import List exposing (..)
import ExType
import ExVariable exposing (rememberVariables)
import ExOperator


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
                ++ (map (\( a, b ) -> a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        -- Primitive operators
        Access ((Variable modules) as left) right ->
            modulePath modules
                ++ "."
                ++ String.join "." (map (toSnakeCase True) right)

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
                |> (flip foldl ( c, "" ) <|
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
                                        ++ name
                                        ++ ", "
                                        ++ produceLambda (cAcc |> rememberVariables [ var ]) args exp

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
            elixirTypeInstances c e


elixirTypeInstances : Context -> Expression -> String
elixirTypeInstances c e =
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
                ++ (map (elixirE c) vars
                        |> String.join ", "
                   )
                ++ "]"

        Tuple vars ->
            "{"
                ++ (map (elixirE c) vars
                        |> String.join ", "
                   )
                ++ "}"

        Record keyValuePairs ->
            "%{"
                ++ (map (\( a, b ) -> a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        _ ->
            notImplemented "expression" e


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


flattenApplication : Expression -> List Expression
flattenApplication application =
    case application of
        Application left right ->
            if isMacro application then
                (flattenApplication left) ++ [ right ]
            else if isTuple application then
                (flattenApplication left) ++ [ right ]
            else
                [ application ]

        other ->
            [ other ]


tupleOrFunction : Context -> Expression -> String
tupleOrFunction c a =
    case flattenApplication a of
        (Application left right) :: [] ->
            elixirE c left ++ ".(" ++ elixirE c right ++ ")"

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
                                    ++ (map (elixirE c) rest |> String.join ", ")
                                    ++ "}"
                           )

                _ ->
                    Debug.crash "Won't ever happen"

        other ->
            Debug.crash ("Shouldn't ever work for" ++ toString other)


aliasFor : Context -> String -> List Expression -> Maybe String
aliasFor c name rest =
    maybeOr (getAlias c name rest) (getType c name rest)


typeAliasOnly : ExContext.Alias -> Maybe ExContext.Alias
typeAliasOnly ({ aliasType } as ali) =
    case aliasType of
        ExContext.TypeAlias ->
            Just ali

        ExContext.Type ->
            Nothing


restOfParams : Context -> List Expression -> String
restOfParams c params =
    params
        |> map (elixirE c)
        |> String.join ").("
        |> (++) ").("
        |> flip (++) ")"


getAlias : Context -> String -> List Expression -> Maybe String
getAlias c name rest =
    ExContext.getAlias c.mod name c
        |> Maybe.andThen typeAliasOnly
        |> Maybe.andThen (ExType.typealiasConstructor [])
        |> Maybe.map (elixirE c >> (++) "(" >> flip (++) (restOfParams c rest))


getType : Context -> String -> List Expression -> Maybe String
getType c name rest =
    ExContext.getType c.mod name c
        |> (Maybe.map <|
                \{ arity } ->
                    let
                        len =
                            length rest

                        dif =
                            arity - len

                        arguments =
                            generateArguments dif

                        varArgs =
                            map (singleton >> Variable) arguments
                    in
                        if arity == 0 then
                            atomize name
                        else if dif >= 0 then
                            (arguments
                                |> map ((++) " fn ")
                                |> map (flip (++) " ->")
                                |> String.join ""
                            )
                                ++ " {"
                                ++ atomize name
                                ++ ", "
                                ++ (map (rememberVariables (rest ++ varArgs) c |> elixirE) (rest ++ varArgs)
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
                                    ++ toString (length rest)
           )


isTuple : Expression -> Bool
isTuple a =
    case a of
        Application a _ ->
            isTuple a

        Variable [ "()" ] ->
            True

        Variable [ name ] ->
            isUpper name

        Variable list ->
            case lastAndRest list of
                ( Just last, _ ) ->
                    isTuple (Variable [ last ])

                _ ->
                    Debug.crash "Shouldn't ever happen"

        other ->
            False


caseE : Context -> Expression -> List ( Expression, Expression ) -> String
caseE c var body =
    "case "
        ++ elixirE c var
        ++ " do"
        ++ (String.join "" (List.map (c |> rememberVariables [ var ] |> caseInstance) body))
        ++ ind (c.indent)
        ++ "end"


caseInstance : Context -> ( Expression, Expression ) -> String
caseInstance c ( left, right ) =
    (ind (c.indent + 1) ++ elixirE (inArgs c) left)
        ++ " ->"
        ++ (ind (c.indent + 2))
        ++ (elixirE (c |> indent |> indent |> rememberVariables [ left ]) right)


lambda : Context -> List Expression -> Expression -> String
lambda c args body =
    case args of
        arg :: rest ->
            "fn("
                ++ elixirE (inArgs c) arg
                ++ ") -> "
                ++ lambda (c |> rememberVariables args) rest body
                ++ " end"

        [] ->
            elixirE c body


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
                            "flip.((&|>/0).())"
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


produceLambda : Context -> List Expression -> Expression -> String
produceLambda c args body =
    case args of
        arg :: rest ->
            "fn("
                ++ (elixirE (inArgs c) arg)
                ++ ") -> "
                ++ produceLambda (c |> rememberVariables [ arg ]) rest body
                ++ " end"

        [] ->
            elixirE c body
