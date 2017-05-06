module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Compiler


has : String -> String -> Expect.Expectation
has expected s =
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
                "" |> has ""

        -- TUPLES
        , test "Tuples w spaces" <|
            \() ->
                "tuple = (1, 2)" |> has "{1, 2}"
        , test "Tuples w/o spaces" <|
            \() ->
                "tuple = ( 1, 2 )" |> has "{1, 2}"
        , test "Nested tuples" <|
            \() ->
                "tuple = (1, (2, 3))" |> has "{1, {2, 3}}"

        -- Currently ((1, 2), 3) will return (1, 2, 3). Error in parser
        -- , test "Other nested tuples" <| \() ->
        --       "tuple = ((1, 2), 3)" |> hasB "{{1, 2}, 3}"
        -- LISTS
        , test "Lists w spaces" <|
            \() ->
                "list = [ 1, 2 ]" |> has "[1, 2]"
        , test "Lists w/o spaces" <|
            \() ->
                "list = [1, 2]" |> has "[1, 2]"
        , test "Nested Lists" <|
            \() ->
                "list = [ 1, [2, 3] ]" |> has "[1, [2, 3]]"
        , test "Other nested list" <|
            \() ->
                "list = [[1, 2], 3]" |> has "[[1, 2], 3]"

        -- Functions
        , test "Application" <|
            \() ->
                "app = a b c d" |> has "a.(b).(c).(d)"
        , test "ffi" <|
            \() ->
                "upcase name = ffi \"String\" \"to_upper\" name " |> has "String.to_upper(name)"
        , test "ffi" <|
            \() ->
                "a f = flambda 2 f " |> has "fn (x1,x2) -> f.(x1).(x2) end"

        , test "function names are snakecased" <|
            \() ->
                "camelCase = 1" |> has "camel_case()"
        , test "function calls are snakecased" <|
            \() ->
                "a = camelCase 1" |> has "camel_case.(1)"

        -- Operators
        , test "Simple ops" <|
            \() ->
                "add = a + b" |> has "a + b"
        , test "Ops as lambda" <|
            \() ->
                "add = (+)" |> has "(&+/0).()"
        , test "Ops as lambda with param" <|
            \() ->
                "add = ((+) 2)" |> has "(&+/0).().(2)"
        , test "Ops as lambda" <|
            \() ->
                "add = map (+) list" |> has "map.((&+/0).()).(list)"

        -- Typespecs
        , test "Typespecs with dependant types" <|
            \() ->
                "sum : (List Int) -> Int" |> has "@spec sum(list(integer)) :: integer"
        , test "Typespecs with functions" <|
            \() ->
                "map : (List a) -> (a -> a) -> (List a)"
                    |> has "map(list(any), (any -> any)) :: list(any)"
        , test "Typespecs with multiple arg functions" <|
            \() ->
                "map : (List a) -> (a -> a -> b) -> (List a)"
                    |> has "map(list(any), (any, any -> any)) :: list(any) "
        , test "Typespecs names are snakecased" <|
            \() ->
                "mapMap : a" |> has "@spec map_map"
        , test "Records in typespecs" <|
            \() ->
                "record : { a : Int, b : String}" |> has "@spec record :: %{a: integer, b: String.t}"
        , test "Remote typespecs" <|
            \() ->
                "f : Remote.Module.Type -> String.T"
                    |> has "f(Remote.Module.type) :: String.t"

        -- Type aliases
        , test "Types" <|
            \() ->
                "type AType = BType | CType" |> has "@type a_type :: :b_type | :c_type"

        -- Records
        , test "Records work" <|
            \() ->
                "a = { a = 1 }" |> has "%{a: 1}"
        , test "Complex records work" <|
            \() ->
                "a = { a = 1, b = 2, c = (a b)}" |> has "%{a: 1, b: 2, c: a.(b)}"
        , test "Updating records work" <|
            \() ->
                "addToA r = {r | a = (r.a + 5), b = 2} " |> has "%{r | a: r.a + 5, b: 2}"

        -- Module Meta
        , test "Module meta" <|
            \() ->
                "meta = [\"use GenServer\", \"@port 100\"]" |> has "use GenServer"
        , test "Module meta" <|
            \() ->
                "meta = [\"use GenServer\", \"@port 100\"]" |> has "@port 100"

        -- Types Are Tuples
        , test "Type application" <|
            \() ->
                "a = Type a b c" |> has "{:type, a, b, c}"
        , test "Type in tuple" <|
            \() ->
                "a = (Type, a, b, c)" |> has "{:type, a, b, c}"
        , test "Remote types" <|
            \() ->
                "a = Remote.Type a b c" |> has "{:type, a, b, c}"
        , test "Remote types in tuples" <|
            \() ->
                "a = (Remote.Type, a, b, c)" |> has "{:type, a, b, c}"

        -- Doctest
        , test "Doctests" <|
            \() ->
                "{-| A equals 1. It just does\n" ++
                "what the hell\n" ++
                "    a == 1\n" ++
                "-}\n" ++
                "a = 1"
                |> has "iex> a\n"
        -- End
        ]
