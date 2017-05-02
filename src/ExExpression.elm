module ExExpression exposing (..)

import Ast.Expression exposing (..)
import Helpers exposing (..)
import Ast.Statement exposing (..)
import ExContext exposing (Context)
import List exposing (..)
import Regex exposing (..)
import ExAlias
import Dict exposing (Dict)


indent : Context -> Context
indent c =
    { c | indent = c.indent + 1 }


deindent : Context -> Context
deindent c =
    { c | indent = c.indent - 1 }


elixirE : Context -> Expression -> String
elixirE c e =
    case e of
        -- Monads and types to tuples
        Application (Variable [ "Just" ]) arg ->
            elixirE c arg

        Variable [] ->
            ""

        Variable [ "Nothing" ] ->
            "nil"

        Variable [ name ] ->
            if isCapitilzed name then
                ExAlias.maybeAlias c.aliases name
                    |> Maybe.map
                        (\a ->
                            case a of
                                TypeConstructor [ name ] _ ->
                                    elixirE c (Variable [ name ])

                                _ ->
                                    Debug.crash
                                        "Only simple type aliases. Sorry"
                        )
                    |> Maybe.withDefault (atomize name)
            else if isOperator name then
                -- We need a curried version, so kernel won't work
                "Elmchemy." ++ translateOperator name ++ "()"
            else
                toSnakeCase name

        Variable list ->
            case lastAndRest list of
                ( Just last, rest ) ->
                    elixirE c (Variable [ last ])

                _ ->
                    Debug.crash "Shouldn't ever happen"
                        String.join
                        "."
                        list

        -- Primitive types
        (Application name arg) as application ->
            tupleOrFunction c application

        Integer value ->
            toString value

        String value ->
            toString value

        List [] ->
            "[]"

        List [ value ] ->
            "[" ++ combineComas c value ++ "]"

        Record keyValuePairs ->
            "%{"
                ++ (map (\( a, b ) -> a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        RecordUpdate name keyValuePairs ->
            "%{"
                ++ toSnakeCase name
                ++ " | "
                ++ (map (\( a, b ) -> a ++ ": " ++ elixirE c b) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        -- Primitive operators
        Access ((Variable (mod :: rest)) as left) right ->
            mod
                ++ "."
                ++ String.join "." rest
                ++ String.join "." right

        Access left right ->
            elixirE c left
                ++ "."
                ++ String.join "." right

        -- Basic operators that are functions in Elixir
        -- Exception, ( "//", "" )
        -- Exception, ( "%", "" )
        -- Exception, ( "rem", "" )
        -- Exception, ( "^", "" )
        BinOp (Variable [ "//" ]) l r ->
            "div(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"

        BinOp (Variable [ "%" ]) l r ->
            "rem(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"

        -- BinOp (Variable [ "rem" ]) l r ->
        --     "rem(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"
        BinOp (Variable [ "^" ]) l r ->
            ":math.pow(" ++ elixirE c l ++ ", " ++ elixirE c r ++ ")"

        -- It's tuple if it wasn't covered by list
        (BinOp (Variable [ "," ]) a b) as binop ->
            "{" ++ combineComas c binop ++ "}"

        -- "{" ++ (elixirE a i) ++ ", " ++ (elixirE b i) ++ "}"
        BinOp (Variable [ "::" ]) a b ->
            "["
                ++ String.join " "
                    (List.map (\a -> elixirE (indent c) a) [ a, Variable [ "|" ], b ])
                ++ "]"

        BinOp (Variable [ op ]) l r ->
            [ elixirE c l, translateOperator op, elixirE c r ]
                |> String.join " "

        -- Primitive expressions
        Case var body ->
            caseE c var body

        Lambda args body ->
            lambda c args body

        (If check onTrue onFalse) as exp ->
            "cond do"
                :: handleIfExp (indent c) exp
                ++ [ ind c.indent, "end" ]
                |> String.join ""

        Let variables expression ->
            variables
                |> map
                    (\( var, exp ) ->
                        var ++ " = " ++ elixirE c exp
                    )
                |> String.join (ind c.indent)
                |> flip (++) (elixirE c expression)

        -- Rest
        e ->
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
        List [ args ] ->
            map
                (getMetaLine)
                (flattenCommas args)
                |> map ((++) (ind 0))
                |> String.join ""
                |> flip (++) "\n"

        _ ->
            Debug.crash "Meta function has to have specific format"


combineComas : Context -> Expression -> String
combineComas c e =
    flattenCommas e
        |> map (elixirE c)
        |> String.join ", "


flattenCommas : Expression -> List Expression
flattenCommas e =
    case e of
        BinOp (Variable [ "," ]) ((BinOp (Variable [ "," ]) l _) as n) r ->
            flattenCommas n ++ [ r ]

        BinOp (Variable [ "," ]) l r ->
            [ l ] ++ [ r ]

        other ->
            [ other ]


isMacro : Expression -> Bool
isMacro e =
    case e of
        Application a _ ->
            isMacro a

        Variable [ "ffi" ] ->
            True

        Variable [ "lffi" ] ->
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
        [ Application left right ] ->
            elixirE c left ++ ".(" ++ elixirE c right ++ ")"

        -- Elmchemy hack
        [ Variable [ "ffi" ], String mod, String fun, (BinOp (Variable [ "," ]) _ _) as args ] ->
            mod ++ "." ++ fun ++ "(" ++ combineComas c args ++ ")"

        -- One arg fun
        [ Variable [ "ffi" ], String mod, String fun, any ] ->
            mod ++ "." ++ fun ++ "(" ++ elixirE c any ++ ")"

        -- Elmchemy hack
        [ Variable [ "lffi" ], String fun, (BinOp (Variable [ "," ]) _ _) as args ] ->
            fun ++ "(" ++ combineComas c args ++ ")"

        -- One arg fun
        [ Variable [ "lffi" ], String fun, any ] ->
            fun ++ "(" ++ elixirE c any ++ ")"

        (Variable [ name ]) :: rest ->
            "{"
                ++ elixirE c (Variable [ name ])
                ++ ", "
                ++ (map (\a -> elixirE c a) rest |> String.join ", ")
                ++ "}"

        (Variable list) :: rest ->
            case lastAndRest list of
                ( Just last, _ ) ->
                    "{"
                        ++ elixirE c (Variable [ last ])
                        ++ ", "
                        ++ (map (elixirE c) rest |> String.join ", ")
                        ++ "}"

                _ ->
                    Debug.crash "Won't ever happen"

        other ->
            Debug.crash ("Shouldn't ever work for" ++ toString other)


isTuple : Expression -> Bool
isTuple a =
    case a of
        Application a _ ->
            isTuple a

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
        ++ " -> "
        ++ (elixirE c (Tuple.second a))


lambda : Context -> List String -> Expression -> String
lambda c args body =
    case args of
        arg :: rest ->
            "fn("
                ++ arg
                ++ ") -> "
                ++ (lambda c rest body)
                ++ " end"

        [] ->
            elixirE c body


genElixirFunc : Context -> String -> List String -> Expression -> String
genElixirFunc c name args body =
    if isOperator name then
        case args of
            [ l, r ] ->
                (ind c.indent)
                    ++ defOrDefp c name
                    ++ l
                    ++ " "
                    ++ translateOperator name
                    ++ " "
                    ++ r
                    ++ " do"
                    ++ (ind <| c.indent + 1)
                    ++ elixirE (indent c) body
                    ++ ind c.indent
                    ++ "end"

            _ ->
                Debug.crash
                    "operator has to have 2 arguments"
    else
        (ind c.indent)
            ++ defOrDefp c name
            ++ toSnakeCase name
            ++ "("
            ++ String.join ", " args
            ++ ") do"
            ++ (ind <| c.indent + 1)
            ++ elixirE (indent c) body
            ++ ind c.indent
            ++ "end"


defOrDefp : Context -> String -> String
defOrDefp context name =
    case context.exports of
        SubsetExport exports ->
            if any (\exp -> exp == FunctionExport name) exports then
                "def "
            else
                "defp "

        AllExport ->
            "def "

        other ->
            Debug.crash "No such export"


(|+) : String -> String -> String
(|+) a b =
    a ++ b


functionCurry : Context -> String -> List String -> String
functionCurry c name args =
    case List.length args of
        0 ->
            ""

        arity ->
            (ind c.indent)
                ++ "curry "
                ++ toSnakeCase name
                ++ "/"
                ++ toString arity


genFunctionDefinition : Context -> String -> List String -> Expression -> String
genFunctionDefinition c name args body =
    functionCurry c name args
        ++ genElixirFunc c name args body
        ++ "\n"


genOverloadedFunctionDefinition :
    Context
    -> String
    -> List String
    -> Expression
    -> List ( Expression, Expression )
    -> String
genOverloadedFunctionDefinition c name args body expressions =
    functionCurry c name args
        ++ (expressions
                |> List.map
                    (\( left, right ) ->
                        genElixirFunc
                            c
                            name
                            [ elixirE c left |> unquoteSplicing ]
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


unquoteSplicing : String -> String
unquoteSplicing =
    Regex.replace All (regex "(^\\{|\\}$)") (\_ -> "")


operators : Dict String String
operators =
    [ ( "||", "||" )
    , ( "&&", "&&" )
    , ( "==", "==" )
    , ( "/=", "!=" )
    , ( "<", "<" )
    , ( ">", ">" )
    , ( ">=", ">=" )
    , ( "<=", "<=" )
    , ( "++", "++" )
    , ( "+", "+" )
    , ( "-", "-" )
    , ( "*", "*" )
    , ( "/", "/" )
    , ( ">>", ">>>" )
    , ( "<|", "<<<" )

    -- Exception
    , ( "%", "" )

    -- Exception
    , ( "//", "" )

    -- Exception
    , ( "rem", "" )

    -- Exception
    , ( "^", "" )
    , ( "<<", "" )
    , ( "|>", "|>" )
    ]
        |> List.foldl (uncurry Dict.insert) Dict.empty


isOperator : String -> Bool
isOperator name =
    operators
        |> Dict.keys
        |> List.any ((==) name)


translateOperator : String -> String
translateOperator op =
    case Dict.get op operators of
        Just "" ->
            Debug.crash
                (op
                    ++ "is not a valid or not implemented yet operator"
                )

        Just key ->
            key

        _ ->
            Debug.crash
                (op
                    ++ "is not a valid or not implemented yet operator"
                )
