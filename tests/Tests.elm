module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Compiler


is : String -> String -> Expect.Expectation
is expected s =
    let
        result =
            Compiler.tree ("module MyModule exposing (..) \n" ++ s)
    in
        (String.contains (String.trim expected) result)
            |> Expect.true ("Code:\n" ++ result ++ "\n\ndoes not contain:\n" ++ expected)


all : Test
all =
    describe "All"
        [ test "Can compile successfully" <|
            \() ->
                "" |> is ""

        -- TUPLES
        , test "Tuples w spaces" <|
            \() ->
                "tuple = (1, 2)" |> is "{1, 2}"
        , test "Tuples w/o spaces" <|
            \() ->
                "tuple = ( 1, 2 )" |> is "{1, 2}"
        , test "Nested tuples" <|
            \() ->
                "tuple = (1, (2, 3))" |> is "{1, {2, 3}}"

        -- Currently ((1, 2), 3) will return (1, 2, 3). Error in parser
        -- , test "Other nested tuples" <| \() ->
        --       "tuple = ((1, 2), 3)" |> is "{{1, 2}, 3}"
        -- LISTS
        , test "Lists w spaces" <|
            \() ->
                "list = [ 1, 2 ]" |> is "[1, 2]"
        , test "Lists w/o spaces" <|
            \() ->
                "list = [1, 2]" |> is "[1, 2]"
        , test "Nested Lists" <|
            \() ->
                "list = [ 1, [2, 3] ]" |> is "[1, [2, 3]]"
        , test "Other nested list" <|
            \() ->
                "list = [[1, 2], 3]" |> is "[[1, 2], 3]"

        -- Functions
        , test "Application" <|
            \() ->
                "app = a b c d" |> is "a.(b).(c).(d)"
        , test "ffi" <|
            \() ->
                "upcase name = ffi \"String\" \"to_upper\" name " |> is "String.to_upper(name)"
        , test "function names are snakecased" <|
            \() ->
                "camelCase = 1" |> is "camel_case()"
        , test "function calls are snakecased" <|
            \() ->
                "a = camelCase 1" |> is "camel_case.(1)"

        -- -- Typespecs
        -- , test "Typespecs with dependant types" <|
        --     \() ->
        --         "sum : (List Int) -> Int" |> is "@spec sum(list(int)) :: int"
        -- , test "Typespecs with functions" <|
        --     \() ->
        --         "map : (List a) -> (a -> a) -> (List a)"
        --             |> is "map(list(any), (any -> any)) :: list(any)"
        -- , test "Typespecs with multiple arg functions" <|
        --     \() ->
        --         "map : (List a) -> (a -> a -> b) -> (List a)"
        --             |> is "map(list(any), (any, any -> any)) :: list(any) "
        -- , test "Typespecs names are snakecased" <|
        --     \() ->
        --         "mapMap : a" |> is "@spec map_map"
        -- , test "Records in typespecs" <|
        --     \() ->
        --         "record : { a : Int, b : String}" |> is "@spec record :: %{a: int, b: String.t}"
        -- -- Type aliases
        -- , test "Types" <|
        --     \() ->
        --         "type AType = BType | CType" |> is "@type a_type :: b_type | c_type"
        -- Records
        , test "Records work" <|
            \() ->
                "a = { a = 1 }" |> is "%{a: 1}"
        , test "Complex records work" <|
            \() ->
                "a = { a = 1, b = 2, c = (a b)}" |> is "%{a: 1, b: 2, c: a.(b)}"
        , test "Updating records work" <|
            \() ->
                "addToA r = {r | a = (r.a + 5), b = 2} " |> is "%{r | a: r.a + 5, b: 2}"

        -- Module Meta
        , test "Module meta" <|
            \() ->
                "meta = [\"use GenServer\", \"@port 100\"]" |> is "use GenServer"
        , test "Module meta" <|
            \() ->
                "meta = [\"use GenServer\", \"@port 100\"]" |> is "@port 100"

        -- Types Are Tuples
        , test "Type application" <|
            \() ->
                "a = Type a b c" |> is "{:type, a, b, c}"
        , test "Type in tuple" <|
            \() ->
                "a = (Type, a, b, c)" |> is "{:type, a, b, c}"
        , test "Remote types" <|
            \() ->
                "a = Remote.Type a b c" |> is "{:type, a, b, c}"
        , test "Remote types in tuples" <|
            \() ->
                "a = (Remote.Type, a, b, c)" |> is "{:type, a, b, c}"

        -- End
        ]
