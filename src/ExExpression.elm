module ExExpression exposing (..)

import Ast.Expression exposing (..)
import Helpers exposing (..)
import Ast.Statement exposing (..)
import ExContext exposing (Context, indent, deindent, onlyWithoutFlag)
import List exposing (..)
import ExAlias
import ExType
import Dict


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

        -- Basic operators that are functions in Elixir
        -- Exception, ( "//", "" )
        -- Exception, ( "%", "" )
        -- Exception, ( "rem", "" )
        -- Exception, ( "^", "" )
        -- Tuple is an exception
        BinOp (Variable [ op ]) l r ->
            elixirBinop c op l r

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
            "cond do"
                :: handleIfExp (indent c) exp
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
                |> map
                    (\( var, exp ) ->
                        case applicationToList var of
                            [ Variable [ name ] ] ->
                                toSnakeCase True name
                                    ++ " = "
                                    ++ elixirE c exp

                            (Variable [ name ]) :: args ->
                                toSnakeCase True name
                                    ++ " = "
                                    ++ produceLambda c args exp

                            [ assign ] ->
                                elixirE c assign
                                    ++ " = "
                                    ++ elixirE c exp

                            _ ->
                                Debug.crash "Impossible"
                    )
                |> String.join (ind c.indent)
                |> flip (++) (ind c.indent ++ elixirE c expression)

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
            toString value

        String value ->
            unescape (toString value)

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
            (++)
                [ ind c.indent
                , elixirE (indent c) check
                , " -> "
                , elixirE (indent c) onTrue
                ]
                (handleIfExp c onFalse)

        _ ->
            [ ind c.indent
            , "true -> "
            , elixirE (indent c) e
            ]


getMetaLine : Expression -> String
getMetaLine a =
    case a of
        String line ->
            line

        _ ->
            Debug.crash "Meta function has to have specific format"


generateMeta : Expression -> String
generateMeta e =
    case e of
        List args ->
            (map getMetaLine args
                |> map ((++) (ind 0))
                |> String.join ""
                |> flip (++) "\n"
            )
                ++ "\n"

        _ ->
            Debug.crash "Meta function has to have specific format"


flambdify : Context -> List (List Type) -> String
flambdify c argTypes =
    let
        arity =
            length argTypes - 1

        indexes =
            range 1 arity
    in
        map2 (,) indexes argTypes
            |> map
                (\( i, arg ) ->
                    case arg of
                        [] ->
                            Debug.crash "Impossible"

                        [ any ] ->
                            "a" ++ toString i

                        list ->
                            resolveFfi c (Flambda (length list - 1) (Variable [ "a" ++ toString i ]))
                )
            |> String.join ", "


generateFfi : Context -> String -> List (List Type) -> Expression -> String
generateFfi c name argTypes e =
    let
        typeDef =
            c.definitions |> Dict.get name

        appList =
            applicationToList e

        flambdaArguments =
            flambdify c argTypes
    in
        case ( typeDef, applicationToList e ) of
            ( Nothing, _ ) ->
                Debug.crash "Ffi requires type definition"

            ( Just def, [ Variable [ "ffi" ], String mod, String fun ] ) ->
                functionCurry c name def.arity
                    ++ (onlyWithoutFlag c
                            "noverify"
                            name
                            (ind c.indent
                                ++ "verify as: "
                                ++ mod
                                ++ "."
                                ++ fun
                                ++ "/"
                                ++ toString def.arity
                            )
                       )
                    ++ ind c.indent
                    ++ "def "
                    ++ toSnakeCase True name
                    ++ "("
                    ++ (generateArguments_ "a" def.arity |> String.join ", ")
                    ++ ")"
                    ++ ", do: "
                    ++ mod
                    ++ "."
                    ++ fun
                    ++ "("
                    ++ flambdaArguments
                    ++ ")"

            ( Just def, [ Variable [ "tryFfi" ], String mod, String fun ] ) ->
                functionCurry c name def.arity
                    ++ ind c.indent
                    ++ "def "
                    ++ toSnakeCase True name
                    ++ "("
                    ++ (generateArguments_ "a" def.arity |> String.join ", ")
                    ++ ")"
                    ++ " do "
                    ++ ind (c.indent + 1)
                    ++ "try_catch fn -> "
                    ++ ind (c.indent + 2)
                    ++ mod
                    ++ "."
                    ++ fun
                    ++ "("
                    ++ flambdaArguments
                    ++ ")"
                    ++ ind (c.indent + 1)
                    ++ "end"
                    ++ ind c.indent
                    ++ "end"

            _ ->
                Debug.crash "Wrong ffi definition"


combineComas : Context -> Expression -> String
combineComas c e =
    flattenCommas e
        |> map (elixirE c)
        |> String.join ", "


flattenCommas : Expression -> List Expression
flattenCommas e =
    case e of
        Tuple kvs ->
            kvs

        a ->
            [ a ]


flattenPipes : Expression -> List Expression
flattenPipes e =
    case e of
        BinOp (Variable [ "|>" ]) l ((BinOp (Variable [ "|>" ]) r _) as n) ->
            [ l ] ++ flattenPipes n

        BinOp (Variable [ "|>" ]) l r ->
            [ l ] ++ [ r ]

        other ->
            [ other ]


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


applicationToList : Expression -> List Expression
applicationToList application =
    case application of
        Application left right ->
            (applicationToList left) ++ [ right ]

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
                        |> Maybe.withDefault
                            ("{"
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
    ExAlias.maybeAlias c.aliases name
        |> Maybe.andThen
            (\({ aliasType } as ali) ->
                case aliasType of
                    ExContext.TypeAlias ->
                        Just ali

                    ExContext.Type ->
                        Nothing
            )
        |> Maybe.andThen (ExType.typealiasConstructor [])
        |> Maybe.map
            ((elixirE c)
                >> (++) "("
                >> flip (++)
                    (rest
                        |> map (elixirE c)
                        |> String.join ").("
                        |> (++) ").("
                        |> flip (++) ")"
                    )
            )
        |> maybeOr
            (Dict.get name c.types
                |> Maybe.map
                    (\arity ->
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
                                arguments
                                    |> map ((++) " fn ")
                                    |> map (flip (++) " ->")
                                    |> String.join ""
                                    |> flip (++)
                                        (" {"
                                            ++ atomize name
                                            ++ ", "
                                            ++ (map (elixirE c) (rest ++ varArgs) |> String.join ", ")
                                            ++ "}"
                                        )
                                    |> flip (++) (String.repeat dif " end ")
                            else
                                Debug.crash
                                    ("Expected "
                                        ++ toString arity
                                        ++ " arguments for '"
                                        ++ name
                                        ++ "'. Got: "
                                        ++ toString (length rest)
                                    )
                    )
            )


type Ffi
    = Lffi Expression Expression
    | Ffi Expression Expression Expression
    | TryFfi Expression Expression Expression
    | Flambda Int Expression


resolveFfi : Context -> Ffi -> String
resolveFfi c ffi =
    case ffi of
        TryFfi (String mod) (String fun) ((Tuple _) as args) ->
            "try_catch fn _ -> "
                ++ mod
                ++ "."
                ++ fun
                ++ "("
                ++ combineComas c args
                ++ ")"
                ++ " end"

        -- One or many arg fun
        TryFfi (String mod) (String fun) any ->
            "try_catch fn _ -> "
                ++ mod
                ++ "."
                ++ fun
                ++ "("
                ++ elixirE c any
                ++ ")"
                ++ " end"

        -- Elchemy hack
        Ffi (String mod) (String fun) ((Tuple _) as args) ->
            mod ++ "." ++ fun ++ "(" ++ combineComas c args ++ ")"

        -- One or many arg fun
        Ffi (String mod) (String fun) any ->
            mod ++ "." ++ fun ++ "(" ++ elixirE c any ++ ")"

        -- Elchemy hack
        Lffi (String fun) ((Tuple _) as args) ->
            fun ++ "(" ++ combineComas c args ++ ")"

        -- One arg fun
        Lffi (String fun) any ->
            fun ++ "(" ++ elixirE c any ++ ")"

        Flambda arity fun ->
            let
                args =
                    generateArguments arity
            in
                "fn ("
                    ++ String.join "," args
                    ++ ") -> "
                    ++ elixirE c fun
                    ++ (map (\a -> ".(" ++ a ++ ")") args
                            |> String.join ""
                       )
                    ++ " end"

        _ ->
            Debug.crash "Wrong ffi call"


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
        ++ (String.join ""
                (List.map (caseInstance c) body)
           )
        ++ ind (c.indent)
        ++ "end"


caseInstance : Context -> ( Expression, Expression ) -> String
caseInstance c a =
    (ind (c.indent + 1) ++ elixirE c (Tuple.first a))
        ++ " ->"
        ++ (ind (c.indent + 2))
        ++ (elixirE c (Tuple.second a))


lambda : Context -> List Expression -> Expression -> String
lambda c args body =
    case args of
        arg :: rest ->
            "fn("
                ++ elixirE c arg
                ++ ") -> "
                ++ lambda c rest body
                ++ " end"

        [] ->
            elixirE c body


genElixirFunc : Context -> String -> List Expression -> Expression -> String
genElixirFunc c name args body =
    case ( isOperator name, args ) of
        ( Builtin, [ l, r ] ) ->
            (ind c.indent)
                ++ "def"
                ++ privateOrPublic c name
                ++ " "
                ++ elixirE c l
                ++ " "
                ++ translateOperator name
                ++ " "
                ++ elixirE c r
                ++ " do"
                ++ (ind <| c.indent + 1)
                ++ elixirE (indent c) body
                ++ ind c.indent
                ++ "end"

        ( Custom, _ ) ->
            (ind c.indent)
                ++ "def"
                ++ privateOrPublic c name
                ++ " "
                ++ translateOperator name
                ++ "("
                ++ (args
                        |> List.map (elixirE c)
                        |> String.join ", "
                   )
                ++ ") do"
                ++ (ind <| c.indent + 1)
                ++ elixirE (indent c) body
                ++ ind c.indent
                ++ "end"

        ( Builtin, _ ) ->
            Debug.crash
                ("operator " ++ name ++ " has to have 2 arguments but has " ++ toString args)

        ( None, _ ) ->
            (ind c.indent)
                ++ "def"
                ++ privateOrPublic c name
                ++ " "
                ++ toSnakeCase True name
                ++ "("
                ++ (args
                        |> List.map (elixirE c)
                        |> String.join ", "
                   )
                ++ ") do"
                ++ (ind <| c.indent + 1)
                ++ elixirE (indent c) body
                ++ ind c.indent
                ++ "end"


privateOrPublic : Context -> String -> String
privateOrPublic context name =
    case context.exports of
        SubsetExport exports ->
            if any (\exp -> exp == FunctionExport name) exports then
                ""
            else
                "p"

        AllExport ->
            ""

        other ->
            Debug.crash "No such export"


functionCurry : Context -> String -> Int -> String
functionCurry c name arity =
    case ( arity, ExContext.hasFlag "nocurry" name c ) of
        ( 0, _ ) ->
            ""

        ( _, True ) ->
            ""

        ( arity, False ) ->
            let
                resolvedName =
                    if isOperator name == Custom then
                        translateOperator name
                    else
                        toSnakeCase True name
            in
                (ind c.indent)
                    ++ "curry"
                    ++ privateOrPublic c name
                    ++ " "
                    ++ resolvedName
                    ++ "/"
                    ++ toString arity


genFunctionDefinition : Context -> String -> List Expression -> Expression -> String
genFunctionDefinition c name args body =
    if ExContext.hasFlag "nodef" name c then
        functionCurry c name (length args)
    else
        functionCurry c name (length args)
            ++ genElixirFunc c name args body
            ++ "\n"


genOverloadedFunctionDefinition :
    Context
    -> String
    -> List Expression
    -> Expression
    -> List ( Expression, Expression )
    -> String
genOverloadedFunctionDefinition c name args body expressions =
    if ExContext.hasFlag "nodef" name c then
        functionCurry c name (length args)
    else
        functionCurry c name (length args)
            ++ (expressions
                    |> List.map
                        (\( left, right ) ->
                            genElixirFunc
                                c
                                name
                                [ left ]
                                right
                        )
                    |> List.foldr (++) ""
                    |> flip (++) "\n"
               )


getVariableName : Expression -> String
getVariableName e =
    case e of
        Variable [ name ] ->
            name

        _ ->
            Debug.crash "It's not a variable"


elixirVariable : Context -> List String -> String
elixirVariable c var =
    case var of
        [] ->
            ""

        [ "()" ] ->
            "{}"

        [ "Nothing" ] ->
            "nil"

        [ "curry" ] ->
            "curried"

        [ "uncurry" ] ->
            "uncurried"

        [ name ] ->
            if isCapitilzed name then
                aliasFor c name []
                    |> Maybe.withDefault (atomize name)
            else if String.startsWith "@" name then
                String.dropLeft 1 name
                    |> atomize
            else
                case isOperator name of
                    Builtin ->
                        -- We need a curried version, so kernel won't work
                        if name == "<|" then
                            "flip.((&|>/0).())"
                        else
                            "(&" ++ translateOperator name ++ "/0).()"

                    Custom ->
                        translateOperator name

                    None ->
                        toSnakeCase True name

        list ->
            case lastAndRest list of
                ( Just last, rest ) ->
                    elixirE c (Variable [ last ])

                _ ->
                    Debug.crash "Shouldn't ever happen"
                        String.join
                        "."
                        list


elixirBinop : Context -> String -> Expression -> Expression -> String
elixirBinop c op l r =
    case op of
        "//" ->
            "div(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"

        "%" ->
            "rem(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"

        "^" ->
            ":math.pow(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"

        "::" ->
            "["
                ++ elixirE c l
                ++ " | "
                ++ elixirE c r
                ++ "]"

        "<<" ->
            elixirBinop c ">>" r l

        "<|" ->
            elixirBinop c "|>" r l

        "|>" ->
            elixirE c l
                ++ (flattenPipes r
                        |> map (elixirE c)
                        |> map ((++) (ind c.indent ++ "|> ("))
                        |> map (flip (++) ").()")
                        |> String.join ""
                   )

        op ->
            case isOperator op of
                Builtin ->
                    [ "(", elixirE c l, translateOperator op, elixirE c r, ")" ]
                        |> String.join " "

                Custom ->
                    (translateOperator op)
                        ++ "("
                        ++ elixirE c l
                        ++ ", "
                        ++ elixirE c r
                        ++ ")"

                None ->
                    Debug.crash ("Illegal operator " ++ op)


produceLambda : Context -> List Expression -> Expression -> String
produceLambda c args body =
    case args of
        arg :: rest ->
            "fn("
                ++ (elixirE c arg)
                ++ ") -> "
                ++ produceLambda c rest body
                ++ " end"

        [] ->
            elixirE c body
