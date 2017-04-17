module Compiler exposing (tree)

import Char
import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Tuple exposing (..)
import List as ElmList
import List exposing (..)


-- type alias .T = List Int


version =
    "0.0.8"


type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    }


glueStart : String
glueStart =
    (ind 0)
        ++ "use Elmchemy"
        ++ "\n"


glueEnd : String
glueEnd =
    "\n"
        ++ String.trim
            """
         end
         """


typespec : Type -> String
typespec t =
    typespec_ True t


typespecf : Type -> String
typespecf t =
    typespec_ False t


typespec_ : Bool -> Type -> String
typespec_ start t =
    case t of
        -- Last aruments
        TypeApplication other ((TypeApplication _ _) as app) ->
            (if start then
                "("
             else
                ""
            )
                ++ typespecf other
                ++ typespecf app

        TypeApplication pre_last last ->
            (if start then
                "("
             else
                ""
            )
                ++ elixirT pre_last
                ++ ") :: "
                ++ elixirT last

        --TypeApplication tc t ->
        (TypeConstructor _ _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        (TypeVariable _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        (TypeTuple _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        (TypeRecord _) as t ->
            (if start then
                " :: "
             else
                ""
            )
                ++ elixirT t
                ++ (if start then
                        ""
                    else
                        ", "
                   )

        other ->
            notImplemented "typespec" other


typealias : Type -> String
typealias t =
    case t of
        TypeApplication tc t ->
            typealias tc ++ typealias t

        (TypeConstructor _ _) as t ->
            elixirT t

        TypeVariable name ->
            name

        other ->
            notImplemented "typealias" other


elixirT : Type -> String
elixirT t =
    case t of
        TypeTuple [ a ] ->
            elixirT a

        TypeTuple [ a, b ] ->
            "{" ++ elixirT a ++ ", " ++ elixirT b ++ "}"

        TypeVariable "number" ->
            "number"

        TypeVariable name ->
            "any"

        TypeConstructor [ "String" ] [] ->
            "String.t"

        TypeConstructor [ "Int" ] [] ->
            "int"

        TypeConstructor [ "List" ] [ t ] ->
            "list(" ++ elixirT t ++ ")"

        TypeConstructor [ "Maybe" ] [ t ] ->
            elixirT t ++ " | nil"

        TypeConstructor [ "T" ] [] ->
            "t"

        TypeConstructor [ t ] [] ->
            toSnakeCase t

        TypeConstructor t [] ->
            -- (String.join "." t) ++ ".t"
            case lastAndRest t of
                ( Just last, a ) ->
                    last

                _ ->
                    Debug.crash "Shouldn't ever happen"

        TypeConstructor [ t ] list ->
            "{"
                ++ atomize t
                ++ ", "
                ++ (map elixirT list |> String.join ", ")
                ++ "}"

        TypeRecord fields ->
            "%{"
                ++ (map (\( k, v ) -> k ++ ": " ++ elixirT v) fields
                        |> String.join ", "
                   )
                ++ "}"

        TypeApplication l r ->
            "("
                ++ (flattenTypeApplication r
                        |> lastAndRest
                        |> \( last, rest ) ->
                            (map elixirT (l :: rest) |> String.join ", ")
                                ++ " -> "
                                ++ (Maybe.map elixirT last |> Maybe.withDefault "")
                   )
                ++ ")"

        -- case List.reverse t of
        --     "T" :: rest -> (rest |> reverse |> String.join ".") ++ ".t()"
        --     whole -> (whole |> reverse |> String.join ".")
        other ->
            notImplemented "type" other


uncons : List a -> ( Maybe a, List a )
uncons list =
    case list of
        a :: b ->
            ( Just a, b )

        [] ->
            ( Nothing, [] )


lastAndRest : List a -> ( Maybe a, List a )
lastAndRest list =
    list
        |> List.reverse
        |> uncons
        |> Tuple.mapSecond List.reverse


moduleStatement : Statement -> Context
moduleStatement s =
    case s of
        ModuleDeclaration names exports ->
            Context (String.join "." names) exports 0

        other ->
            Debug.crash "First statement must be module declaration"


ind : Int -> String
ind i =
    "\n" ++ (List.repeat ((i + 1) * 2) " " |> String.join "")


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


elixirS : Statement -> Context -> String
elixirS s c =
    case s of
        TypeDeclaration name types ->
            ""

        -- (ind c.indent)
        --     ++ "@type "
        --     ++ (typealias name)
        --     ++ " :: "
        --     ++ ((map (typealias) types) |> String.join " | ")
        TypeAliasDeclaration _ _ ->
            ""

        --"alias?"
        FunctionTypeDeclaration name t ->
            ""

        -- (ind c.indent) ++ "@spec " ++ toSnakeCase name ++ (typespec t)
        (FunctionDeclaration name args body) as fd ->
            if name == "meta" && args == [] then
                generateMeta body
            else if List.length args > 1 then
                (ind c.indent)
                    ++ "curry "
                    ++ name
                    ++ "/"
                    ++ toString (List.length args)
                    ++ genElixirFunc c name args body
                    ++ "\n"
            else
                case body of
                    Case _ expressions ->
                        (ind c.indent)
                            ++ "curry "
                            ++ name
                            ++ "/1"
                            ++ (expressions
                                    |> List.map (\( left, right ) -> genElixirFunc c name [ elixirE left c.indent ] right)
                                    |> List.foldr (++) ""
                                    |> flip (++) "\n"
                               )

                    _ ->
                        genElixirFunc c name args body
                            ++ "\n"

        Comment content ->
            (ind c.indent) ++ "#" ++ content

        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            (ind c.indent) ++ "alias " ++ String.join "." path

        ImportStatement path Nothing (Just AllExport) ->
            (ind c.indent) ++ "import " ++ String.join "." path

        s ->
            notImplemented "statement" s


defOrDefp : Context -> String -> String
defOrDefp context name =
    case context.exports of
        SubsetExport exports ->
            if any (\exp -> exp == FunctionExport name) exports then
                "def "
            else
                "defp "

        AllExport ->
            "defcurry "

        other ->
            Debug.crash "No such export"


elixirE : Expression -> Int -> String
elixirE e i =
    case e of
        -- Monads and types to tuples
        Application (Variable [ "Err" ]) arg ->
            "{:error, " ++ tupleOrFunction arg i ++ "}"

        Application (Variable [ "Ok" ]) arg ->
            "{:ok, " ++ tupleOrFunction arg i ++ "}"

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
                    atomize last

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


tree : String -> String
tree m =
    case Ast.parse m of
        Ok ( _, _, first :: statements ) ->
            let
                context =
                    moduleStatement first
            in
                ("# Compiled using Elmchemy v" ++ version)
                    ++ "\n"
                    ++ ("defmodule " ++ context.mod ++ " do")
                    ++ glueStart
                    ++ ((List.map (\a -> elixirS a context) statements)
                            |> (List.foldr (++) "")
                       )
                    ++ glueEnd

        Err ( (), { input, position }, [ msg ] ) ->
            "]ERR> Compilation error at: "
                ++ input
                ++ "\n"

        err ->
            Debug.crash (toString err)


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


type MaybeUpper
    = Upper String
    | Lower String


maybeUpper : String -> MaybeUpper
maybeUpper string =
    case String.uncons string of
        Just ( start, rest ) ->
            if Char.isUpper start then
                Upper string
            else
                Lower string

        Nothing ->
            Lower ""


capitalize : String -> String
capitalize s =
    String.uncons s
        |> Maybe.map (\a -> String.cons (Char.toUpper (first a)) (second a))
        |> Maybe.withDefault ""


atomize : String -> String
atomize s =
    ":" ++ toSnakeCase s


isCapitilzed : String -> Bool
isCapitilzed s =
    String.uncons s
        |> Maybe.map (\a -> a |> first |> Char.isUpper)
        |> Maybe.withDefault False


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

        other ->
            False


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

        other ->
            Debug.crash ("Shouldn't ever work for" ++ toString other)


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


flattenTypeApplication : Type -> List Type
flattenTypeApplication application =
    case application of
        TypeApplication left right ->
            (flattenTypeApplication left) ++ [ right ]

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


notImplemented : String -> a -> String
notImplemented feature value =
    " ## ERROR: No "
        ++ feature
        ++ " implementation for "
        ++ toString value
        ++ " yet"
        ++ "\n"
        |> Debug.crash


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


toSnakeCase : String -> String
toSnakeCase s =
    let
        res =
            toSnakeCase_ s
    in
        if String.startsWith "_" res && res /= "_" then
            res |> String.dropLeft 1
        else
            res


toSnakeCase_ : String -> String
toSnakeCase_ e =
    let
        withUpper =
            \a -> ( first a |> Char.isUpper, first a, second a )
    in
        case String.uncons e |> Maybe.map withUpper of
            Just ( True, a, rest ) ->
                "_" ++ String.cons (Char.toLower a) (toSnakeCase_ rest)

            Just ( False, a, rest ) ->
                String.cons a (toSnakeCase_ rest)

            Nothing ->
                ""


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
