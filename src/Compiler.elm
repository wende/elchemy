module Compiler exposing (tree)

import Char
import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)

glueStart : String
glueStart =
    String.trim """
import Elmchemist.Glue\n
                 """ ++ "\n"
glueEnd : String
glueEnd =
    "\n" ++ String.trim
        """
         end
         """


typespec : Type -> String
typespec t =
    case t of
        TypeApplication tc t ->
            typespec tc ++ typespec t
        TypeConstructor [t] [] ->
            " -> " ++ t
        other ->
            ""

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
             (ind i) ++ "defun " ++ a
                 ++ "(" ++ (String.join "," b) ++ ") do" ++ (ind <| i + 1)
                 ++ (elixirE e (i + 1)) ++ (ind i) ++ "end\n"
        ModuleDeclaration [name] _ ->
            "defmodule " ++ name ++ " do"
        Comment content ->
            (ind i) ++ "%% " ++ content
        s ->
            "Not implemented yet for statement: " ++ toString s

elixirE : Expression -> Int -> String
elixirE e i =
    case e of
        Application (Variable ["Just"]) arg -> "{:ok, " ++ nestedTuple arg i True ++ "}"
        Application name args as application -> nestedTuple application i False
        --  Application (Variable ["Just"]) x -> "{:ok, " ++ nestedTuple x i ++ "}"
        -- Application (Variable [name]) args ->
        --     case maybeUpper name of
        --         Upper name ->
        --             "{:" ++ name ++ ", " ++  elixirE args i ++ "}"
        --         Lower name ->
        --             name  ++ ".(" ++ elixirE args (i + 1) ++ ")"
        -- Application fun args as app ->
        --     nestedTuple app i
        --     --elixirE fun (i + 1) ++ ".(" ++ elixirE args (i + 1) ++ ")"
        Integer value ->
            toString value
        -- Maybe.Just a ->
        --     "{:ok, " ++ a ++ "}"
        String value ->
            toString value
        Variable ["Nothing"] -> "{:error, " ++ nothing ++ "}"
        Variable [name] ->
            name
        BinOp (Variable ["::"]) a b ->
            "[" ++ String.join " " (List.map (\a -> elixirE a (i + 1)) [a, Variable ["|"], b] ) ++ "]"
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

isTuple a =
    case a of
        Application a _ -> isTuple a
        Variable [name] ->
            case maybeUpper name of
                Upper _ -> True
                Lower _ -> False
        other -> False

nestedTuple a i alreadyIn =
    let
        (prefix, suffix) = if isTuple a && not alreadyIn then ("{", "}") else ("", "")
    in
    prefix ++ (case a of
        Application (Variable [name]) args ->
            case maybeUpper name of
                Upper name ->
                    ":" ++ (String.toLower name) ++ ", " ++  elixirE args i
                Lower name ->
                    name  ++ ".(" ++ elixirE args (i + 1) ++ ")"
        -- Application fun args as app ->
        --     nestedTuple app i
        Application (Application name _ as app) args ->
            (nestedTuple app i True) ++ ", " ++ elixirE args i
        Application name args ->
            elixirE name i ++ "," ++ nestedTuple args i True
        other -> elixirE (Debug.log "Other" other) i) ++ suffix


nothing : String
nothing = "\"No value in Maybe\""
