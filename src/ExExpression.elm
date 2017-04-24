module ExExpression exposing (..)

import Ast.Expression exposing (..)
import Helpers exposing (..)
import Ast.Statement exposing (..)
import ExContext exposing (Context)
import List exposing (..)
import Regex exposing (..)
import ExAlias


indent : Context -> Context
indent c =
    { c | indent = c.indent - 1 }


deindent : Context -> Context
deindent c =
    { c | indent = c.indent - 1 }


elixirE : Context -> Expression -> String
elixirE c e =
    case e of
        -- Monads and types to tuples
        Application (Variable [ "Just" ]) arg ->
            elixirE c e

        Application (Variable [ "Err" ]) arg ->
            "{:error, " ++ elixirE c e ++ "}"

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
                                    atomize name

                                _ ->
                                    Debug.crash "Works only for type constructors"
                        )
                    |> Maybe.withDefault (atomize name)
            else
                toSnakeCase name

        -- Variable list ->
        --     case lastAndRest list of
        --         ( Just last, rest ) ->
        --             elixirE c (Variable [ last ])
        --         _ ->
        --             Debug.crash "Shouldn't ever happen"
        --            String.join "." list
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

        -- Basics
        BinOp (Variable [ "/=" ]) l r ->
            elixirE c l ++ " != " ++ elixirE c r

        -- It's tuple if it wasn't covered by list
        (BinOp (Variable [ "," ]) a b) as binop ->
            "{" ++ combineComas c binop ++ "}"

        -- "{" ++ (elixirE a i) ++ ", " ++ (elixirE b i) ++ "}"
        BinOp (Variable [ "::" ]) a b ->
            "["
                ++ String.join " "
                    (List.map (\a -> elixirE (indent c) a) [ a, Variable [ "|" ], b ])
                ++ "]"

        BinOp op a b ->
            (List.map (\a -> elixirE (indent c) a) [ a, op, b ])
                |> String.join " "

        -- Primitive expressions
        Case var body ->
            caseE c var body

        Lambda args body ->
            lambda c args body

        -- Rest
        e ->
            notImplemented "expression" e


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
        ++ ind (c.indent - 1)
        ++ "end"


caseInstance : Context -> ( Expression, Expression ) -> String
caseInstance c a =
    (ind c.indent ++ elixirE c (Tuple.first a))
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
    (ind c.indent)
        ++ (defOrDefp c name)
        ++ toSnakeCase name
        ++ "("
        ++ (String.join ", " args)
        ++ ") do"
        ++ (ind <| c.indent + 1)
        ++ (elixirE (indent c) body)
        ++ (ind c.indent)
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
    (ind c.indent)
        ++ "curry "
        ++ toSnakeCase name
        ++ "/"
        ++ toString (List.length args)


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
