module Compiler exposing (tree)

import Char
import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Tuple exposing (..)
import List exposing (..)

-- type alias .T = List Int

glueStart : String
glueStart =
    String.trim """
use Elmchemist.Glue\n
                 """ ++ "\n"
glueEnd : String
glueEnd =
    "\n" ++ String.trim
        """
         end
         """

typespec : Type -> String
typespec t = typespec_ True t

typespecf : Type -> String
typespecf t = typespec_ False t

typespec_ : Bool -> Type -> String
typespec_ start t =
    case t of
        -- Last aruments
        TypeApplication (TypeConstructor pre_last []) (TypeConstructor last []) ->
            (if start then "(" else "") ++
            elixirT pre_last ++ ") :: " ++ elixirT last
        TypeApplication tc t ->
            (if start then "(" else "") ++
            typespecf tc ++ typespecf t
        TypeConstructor t [] ->
            (if start then " :: " else "") ++
            elixirT t ++
            (if start then "" else ", ")
        other ->  "Not implemented for " ++ (toString other)

elixirT : List String -> String
elixirT t =
    case t of
        ["Int"] -> "int()"
        t ->
            case List.reverse t of
                "T" :: rest -> (rest |> reverse |> String.join ".") ++ ".t()"
                whole -> (whole |> reverse |> String.join ".")

statement : Statement -> String
statement s =
    elixirS s 0

ind : Int -> String
ind i = "\n" ++ (List.repeat ((i + 1)*2) " " |> String.join "")

elixirS : Statement -> Int -> String
elixirS s i =
    case s of
        TypeDeclaration _ _ ->
            ""
        TypeAliasDeclaration _ _ ->
            ""
        FunctionTypeDeclaration name t ->
            (ind i) ++ "@spec " ++ name ++ (typespec t)
        FunctionDeclaration a b e ->
             (ind i) ++ "defcurry " ++ a
                 ++ "(" ++ (String.join "," b) ++ ") do" ++ (ind <| i + 1)
                 ++ (elixirE e (i + 1)) ++ (ind i) ++ "end\n"
        ModuleDeclaration [name] _ ->
            "defmodule " ++ name ++ " do"
        Comment content ->
            (ind i) ++ "# " ++ content
        -- That's not a real import. In elixir it's called alias
        ImportStatement path Nothing Nothing ->
            (ind i) ++ "alias " ++ String.join "." path
        ImportStatement path Nothing (Just AllExport) ->
            elixirS (ImportStatement path Nothing Nothing) i
        s ->
            "Not implemented yet for statement: " ++ toString s

elixirE : Expression -> Int -> String
elixirE e i =
    case e of
        Application (Variable ["Err"]) arg ->
            "{:error, " ++ nestedTuple arg i ++ "}"

        Application (Variable ["Ok"]) arg ->
            "{:ok, " ++ nestedTuple arg i ++ "}"

        Application (Variable ["Just"]) arg ->
            elixirE arg i

        Variable ["Nothing"] ->
            "nil"

        Variable [name] ->
            if isCapitilzed name then
                atomize name
            else
                name

        Application name arg as application ->
            nestedTuple application i

        Integer value ->
            toString value

        String value ->
            toString value

        BinOp (Variable ["::"]) a b ->
            "[" ++
            String.join " "
                (List.map (\a -> elixirE a (i + 1)) [a, Variable ["|"], b] ) ++
            "]"

        BinOp op a b ->
            String.join " " (List.map (\a -> elixirE a (i + 1)) [a, op, b] )

        Case var body ->
            caseE var body (i + 1)

        Lambda args body ->
            lambda args body i

        e ->
            "Not implemented yet for expression: " ++ toString e

lambda : (List String) -> Expression -> Int -> String
lambda args body i =
    case args of
        arg :: rest ->
            "fn(" ++ arg ++ ") -> "
            ++ (lambda rest body i)
            ++ " end"
        [] ->
            elixirE body i

tree : String -> String
tree m =
    case Ast.parse m of
        ( Ok (_, _, statements)) ->
            ((List.map statement statements) |> (List.foldr (++) "")
                         |> flip (++) glueEnd
                         |> (++) glueStart)
        err ->
            toString err

caseE : Expression -> List (Expression, Expression) -> Int -> String
caseE var body i =
    "case " ++ elixirE var i ++ " do" ++
        (String.join ""
             (List.map (caseInstance i) body))
        ++ (ind (i - 1)) ++ "end"

caseInstance : Int -> (Expression, Expression) -> String
caseInstance i a =
    (ind i ++ elixirE (Tuple.first a) i)
    ++ " -> "
    ++ (elixirE (Tuple.second a) i)

type MaybeUpper = Upper String | Lower String
maybeUpper : String -> MaybeUpper
maybeUpper string =
    case String.uncons string of
        Just(start, rest) ->
            if Char.isUpper start then Upper string
            else Lower string
        Nothing -> Lower ""

capitalize : String -> String
capitalize s =
    String.uncons s
        |> Maybe.map (\a -> String.cons (Char.toUpper (first a)) (second a))
        |> Maybe.withDefault ""

atomize : String -> String
atomize s =
    ":" ++ String.toLower s

isCapitilzed : String -> Bool
isCapitilzed s =
    String.uncons s
        |> Maybe.map (\a -> a |> first |> Char.isUpper)
        |> Maybe.withDefault False

isTuple : Expression -> Bool
isTuple a =
    case a of
        Application a _ -> isTuple a
        Variable [name] ->
            case maybeUpper name of
                Upper _ -> True
                Lower _ -> False
        other -> False

nestedTuple : Expression -> Int -> String
nestedTuple a i  =
    case flattenApplication a of
        [Application left right] ->
            elixirE left i ++ ".(" ++ elixirE right i ++ ")"

        Variable [name] :: rest ->
            "{:"
            ++ String.toLower name
            ++ ", "
            ++ (map (\a -> elixirE a i) rest |> String.join ", ")
            ++ "}"

        other -> Debug.crash ("Shouldn't ever work for" ++ toString other)

flattenApplication : Expression -> List Expression
flattenApplication application =
    case application of
        Application left right ->
            if isTuple application then
                (flattenApplication left) ++ [right]
            else
                [application]
        other -> [other]

nothing : String
nothing = "\"No value in Maybe\""

notImplemented : a -> String
notImplemented value =
    "[ERROR: No implementation for " ++ toString value ++ "yet]"
