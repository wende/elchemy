module Example exposing (..)
import Ast.Statement
import Elmchemy exposing (..)
import String

type alias Record = {a: Int}

join : List String -> Int -> String
join list a = "1" ++  ffi "Enum" "join" (list, (a, a))

test1 a b = a + Tuple.first b + Tuple.second b

test : Int -> String
test a =
    toString (test1 1, (1, 2)) ++ join ["a", "b"] 1
