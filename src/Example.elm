module Example exposing (..)
import Ast.Statement
import Elmchemy exposing (..)
import String

type alias Record = {a: Int}

join : List String -> Int -> String
join list a = "1" ++  ffi "Enum" "join" (list, (a, a))

test1 : number -> (number, number) -> number
test1 a b = a

test : Int -> Int -> Int -> List Int -> String
test a b c d =
    toString (test1 1, (1, 2)) ++ join ["a", "b"] 1
