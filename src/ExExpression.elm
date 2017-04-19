module ExExpression exposing (..)

import Ast.Expression exposing (..)
import Helpers exposing (..)
import Ast.Statement exposing (..)
import ExContext exposing (Context)
import List exposing (..)


elixirE : Expression -> Int -> String
elixirE e i =
    case e of
        -- Monads and types to tuples
        Application (Variable [ "Just" ]) arg ->
            elixirE arg i

        Variable [] ->
            ""

        Variable [ "Nothing" ] ->
            "nil"

        Variable [ name ] ->
            if isCapitilzed name then
                atomize name
            else
                toSnakeCase name

        Variable list ->
            case lastAndRest list of
                ( Just last, rest ) ->
                    elixirE (Variable [ last ]) i

                _ ->
                    Debug.crash "Shouldn't ever happen"

        --            String.join "." list
        -- Primitive types
        (Application name arg) as application ->
            tupleOrFunction application i

        Integer value ->
            toString value

        String value ->
            toString value

        List [] ->
            "[]"

        List [ value ] ->
            "[" ++ combineComas value ++ "]"

        Record keyValuePairs ->
            "%{"
                ++ (map (\( a, b ) -> a ++ ": " ++ elixirE b i) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        RecordUpdate name keyValuePairs ->
            "%{"
                ++ toSnakeCase name
                ++ " | "
                ++ (map (\( a, b ) -> a ++ ": " ++ elixirE b i) keyValuePairs
                        |> String.join ", "
                   )
                ++ "}"

        -- Primitive operators
        Access left [ right ] ->
            elixirE left i ++ "." ++ right

        -- Basics
        BinOp (Variable [ "/=" ]) l r ->
            elixirE l i ++ " != " ++ elixirE r i

        -- It's tuple if it wasn't covered by list
        (BinOp (Variable [ "," ]) a b) as binop ->
            "{" ++ combineComas binop ++ "}"

        -- "{" ++ (elixirE a i) ++ ", " ++ (elixirE b i) ++ "}"
        BinOp (Variable [ "::" ]) a b ->
            "["
                ++ String.join " "
                    (List.map (\a -> elixirE a (i + 1)) [ a, Variable [ "|" ], b ])
                ++ "]"

        BinOp op a b ->
            String.join " " (List.map (\a -> elixirE a (i + 1)) [ a, op, b ])

        -- Primitive expressions
        Case var body ->
            caseE var body (i + 1)

        Lambda args body ->
            lambda args body i

        -- Rest
        e ->
            notImplemented "expression" e


generateMeta : Expression -> String
generateMeta e =
    case e of
        List [ args ] ->
            map
                (\a ->
                    case a of
                        String line ->
                            line

                        _ ->
                            Debug.crash "Meta function has to have specific format"
                )
                (flatCommas args)
                |> map ((++) (ind 0))
                |> String.join ""

        _ ->
            Debug.crash "Meta function has to have specific format"


combineComas : Expression -> String
combineComas e =
    flatCommas e
        |> map ((flip elixirE) 0)
        |> String.join ", "


flatCommas : Expression -> List Expression
flatCommas e =
    case e of
        BinOp (Variable [ "," ]) ((BinOp (Variable [ "," ]) l _) as n) r ->
            flatCommas n ++ [ r ]

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


tupleOrFunction : Expression -> Int -> String
tupleOrFunction a i =
    case flattenApplication a of
        [ Application left right ] ->
            elixirE left i ++ ".(" ++ elixirE right i ++ ")"

        -- Elmchemy hack
        [ Variable [ "ffi" ], String mod, String fun, (BinOp (Variable [ "," ]) _ _) as args ] ->
            mod ++ "." ++ fun ++ "(" ++ combineComas args ++ ")"

        -- One arg fun
        [ Variable [ "ffi" ], String mod, String fun, any ] ->
            mod ++ "." ++ fun ++ "(" ++ elixirE any i ++ ")"

        -- Elmchemy hack
        [ Variable [ "lffi" ], String fun, (BinOp (Variable [ "," ]) _ _) as args ] ->
            fun ++ "(" ++ combineComas args ++ ")"

        -- One arg fun
        [ Variable [ "lffi" ], String fun, any ] ->
            fun ++ "(" ++ elixirE any i ++ ")"

        (Variable [ name ]) :: rest ->
            "{"
                ++ atomize name
                ++ ", "
                ++ (map (\a -> elixirE a i) rest |> String.join ", ")
                ++ "}"

        (Variable list) :: rest ->
            case lastAndRest list of
                ( Just last, _ ) ->
                    "{"
                        ++ atomize last
                        ++ ", "
                        ++ (map (\a -> elixirE a i) rest |> String.join ", ")
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
            case maybeUpper name of
                Upper _ ->
                    True

                Lower _ ->
                    False

        Variable list ->
            case lastAndRest list of
                ( Just last, _ ) ->
                    isTuple (Variable [ last ])

                _ ->
                    Debug.crash "Shouldn't ever happen"

        other ->
            False


caseE : Expression -> List ( Expression, Expression ) -> Int -> String
caseE var body i =
    "case "
        ++ elixirE var i
        ++ " do"
        ++ (String.join ""
                (List.map (caseInstance i) body)
           )
        ++ (ind (i - 1))
        ++ "end"


caseInstance : Int -> ( Expression, Expression ) -> String
caseInstance i a =
    (ind i ++ elixirE (Tuple.first a) i)
        ++ " -> "
        ++ (elixirE (Tuple.second a) i)


lambda : List String -> Expression -> Int -> String
lambda args body i =
    case args of
        arg :: rest ->
            "fn("
                ++ arg
                ++ ") -> "
                ++ (lambda rest body i)
                ++ " end"

        [] ->
            elixirE body i


genElixirFunc : Context -> String -> List String -> Expression -> String
genElixirFunc c name args body =
    (ind c.indent)
        ++ (defOrDefp c name)
        ++ toSnakeCase name
        ++ "("
        ++ (String.join ", " args)
        ++ ") do"
        ++ (ind <| c.indent + 1)
        ++ (elixirE body (c.indent + 1))
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


functionCurry c name args =
    (ind c.indent)
        ++ "curry "
        ++ name
        ++ "/"
        ++ toString (List.length args)


genFunctionDefinition c name args body =
    functionCurry c name args
        ++ genElixirFunc c name args body
        ++ "\n"


genOverloadedFunctionDefinition c name args body expressions =
    functionCurry c name args
        ++ (expressions
                |> List.map
                    (\( left, right ) ->
                        genElixirFunc c name [ elixirE left c.indent ] right
                    )
                |> List.foldr (++) ""
                |> flip (++) "\n"
           )
