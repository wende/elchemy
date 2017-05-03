module ExExpression exposing (..)

import Ast.Expression exposing (..)
import Helpers exposing (..)
import Ast.Statement exposing (..)
import ExContext exposing (Context, indent, deindent)
import List exposing (..)
import ExAlias


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
        -- Tuple is an exception
        (BinOp (Variable [ "," ]) a b) as binop ->
            "{" ++ combineComas c binop ++ "}"

        BinOp (Variable [ op ]) l r ->
            elixirBinop c op l r

        -- Rest
        e ->
            elixirPrimitives c e


elixirTypeInstances : Context -> Expression -> String
elixirTypeInstances c e =
    case e of
        Integer value ->
            toString value

        Float value ->
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

        _ ->
            notImplemented "expression" e


elixirPrimitives : Context -> Expression -> String
elixirPrimitives c e =
    case e of
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

        _ ->
            elixirTypeInstances c e


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
        (Application left right) :: rest ->
            elixirE c left ++ ".(" ++ elixirE c right ++ ")"

        (Variable [ "ffi" ]) :: rest ->
            case rest of
                [ mod, fun, args ] ->
                    resolveFfi c (Ffi mod fun args)

                _ ->
                    Debug.crash "Wrong ffi"

        (Variable [ "lffi" ]) :: rest ->
            case rest of
                [ fun, args ] ->
                    resolveFfi c (Lffi fun args)

                _ ->
                    Debug.crash "Wrong lffi"

        [ Variable [ "Just" ], arg ] ->
            elixirE c arg

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


type Ffi
    = Lffi Expression Expression
    | Ffi Expression Expression Expression


resolveFfi : Context -> Ffi -> String
resolveFfi c ffi =
    case ffi of
        -- Elmchemy hack
        Ffi (String mod) (String fun) ((BinOp (Variable [ "," ]) _ _) as args) ->
            mod ++ "." ++ fun ++ "(" ++ combineComas c args ++ ")"

        -- One arg fun
        Ffi (String mod) (String fun) any ->
            mod ++ "." ++ fun ++ "(" ++ elixirE c any ++ ")"

        -- Elmchemy hack
        Lffi (String fun) ((BinOp (Variable [ "," ]) _ _) as args) ->
            fun ++ "(" ++ combineComas c args ++ ")"

        -- One arg fun
        Lffi (String fun) any ->
            fun ++ "(" ++ elixirE c any ++ ")"

        _ ->
            Debug.crash "Wrong ffi call"


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


elixirVariable : Context -> List String -> String
elixirVariable c var =
    case var of
        [] ->
            ""

        [ "Nothing" ] ->
            "nil"

        [ "curry" ] ->
            "curried"

        [ "uncurry" ] ->
            "uncurried"

        [ name ] ->
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
                ++ "|"
                ++ elixirE c r
                ++ "]"

        op ->
            [ elixirE c l, translateOperator op, elixirE c r ]
                |> String.join " "
