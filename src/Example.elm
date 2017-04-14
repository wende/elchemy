module Example exposing (..)
import Ast.Statement
import Elmchemy exposing (..)
import String

type alias Record = {a: Int}

join : List String -> Int -> String
join list a = "1" ++  ffi "Enum" "join" (list, (a, a))

test1 : number -> (number, number) -> number
test1 a b = a

-- Make
add a b = a + b

testName : Int -> Int -> Int -> List Int -> String
testName a b c d =
    List.foldl (add) 0 [b]
             |> toString
