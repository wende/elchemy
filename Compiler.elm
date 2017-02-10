module Main exposing (main)

import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Html exposing (..)
import Html
import Html.Events exposing (..)
import Json.Decode as JD


type Msg
    = Replace String


glueStart : String
glueStart =
    String.trim """

                 """ ++ "\n"
glueEnd : String
glueEnd =
    String.trim
        """
         end
         """

init : String
init =
    """module Main exposing (..)
f : Int -> Int
f x = x + 1
add : Int -> Int -> Int
add a b = a + Just b
h = wende g + hajto cichocinski 10
casa t =
    case t of
        wende -> 1
        cichocinski ->
            \\a -> 1 + 2
-- (Notice that record types use colon and record values use equals.)

-- You can give existing types a nice name with a type alias.
type alias Point3D =
  { x : Float, y : Float, z : Float }

-- If you alias a record, you can use the name as a constructor function.
otherOrigin : Point3D
otherOrigin =
  Point3D 0 0 0

-- But it's still the same type, so you can equate them.

-- By contrast, defining a union type creates a type that didn't exist before.
-- A union type is so called because it can be one of many possibilities.
-- Each of the possibilities is represented as a "tag".
type Direction =
  North | South | East | West

-- Tags can carry other values of known type. This can work recursively.
type IntTree =
  Leaf | Node Int IntTree IntTree
-- "Leaf" and "Node" are the tags. Everything following a tag is a type.

-- Tags can be used as values or functions.
root : IntTree
root =
  Node 7 Leaf Leaf

-- Union types (and type aliases) can use type variables.
type Tree a =
  Leaf | Node a (Tree a) (Tree a)
-- "The type tree-of-a is a leaf, or a node of a, tree-of-a, and tree-of-a."

-- Pattern match union tags. The uppercase tags will be matched exactly. The
-- lowercase variables will match anything. Underscore also matches anything,
-- but signifies that you aren't using it.
leftmostElement : Tree a -> Maybe a
leftmostElement tree =
  case tree of
    Leaf -> Nothing
    Node x Leaf _ -> Just x
    Node _ subtree _ -> leftmostElement subtree


"""


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m


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
ind i = List.repeat i "\t" |> String.join ""

elixirS : Statement -> Int -> String
elixirS s ind=
    case s of
        TypeDeclaration _ _ ->
            "\n"
        TypeAliasDeclaration _ _ ->
            "\n"
        FunctionTypeDeclaration name t ->
            "@spec " ++ name ++ (typespec t)  ++ "\n"
        FunctionDeclaration a b e ->
             "defun " ++ a
                 ++ "(" ++ (String.join "," b) ++ ") do\n\t"
                 ++ (elixirE e (ind + 1)) ++ "\nend\n"
        ModuleDeclaration [name] _ ->
            "defmodule " ++ name ++ " do\n"
        Comment content ->
            "%% " ++ content ++ "\n"
        s ->
            "Not implemented yet for statement: " ++ toString s

elixirE : Expression -> Int -> String
elixirE e i =
    case e of
        Application fun args ->
            elixirE fun (i + 1) ++ ".(" ++ elixirE args (i + 1) ++ ")"
        Integer value ->
            toString value
        -- Maybe.Just a ->
        --     "{:ok, " ++ a ++ "}"
        String value ->
            toString value
        Variable [name] ->
            name
        BinOp (Variable ["::"]) a b ->
            "[" ++ String.join " " (List.map (\a -> elixirE a (i + 1)) [a, Variable ["|"], b] ) ++ "]"
        BinOp op a b ->
            String.join " " (List.map (\a -> elixirE a (i + 1)) [a, op, b] )
        Case var body ->
            caseE var body i
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
tree : String -> Html Msg
tree m =
    case Ast.parse m of
        ( Ok (_, _, statements)) ->
            pre [] [text ((List.map statement statements) |> (List.foldr (++) "")
                         |> flip (++) glueEnd
                         |> (++) glueStart)]

        err ->
            div [] [ text <| toString err ]


view : String -> Html Msg
view model =
    div []
        [ textarea [ on "input" (JD.map Replace targetValue) ] [ text model ]
        , tree model
        ]


main : Program Never String Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


caseE : Expression -> List (Expression, Expression) -> Int -> String
caseE var body i =
    "case " ++ elixirE var i ++ " do\n" ++
        (String.join "\n"
             (List.map (caseInstance i) body))
        ++ "\n end\n"

caseInstance : Int -> (Expression, Expression) -> String
caseInstance i a =
    (ind i ++ elixirE (Tuple.first a) (i + 1))
    ++ " -> "
    ++ (elixirE (Tuple.second a) (i + 1))
