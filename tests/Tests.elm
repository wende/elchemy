module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Compiler
import Regex exposing (..)


(|++) : String -> String -> String
(|++) l r =
    l ++ "\n" ++ r


hasFull expected s =
    let
        result =
            Compiler.tree s
    in
        String.contains (String.trim expected) result
            |> Expect.true ("Code:\n" ++ result ++ "\n\ndoes not contain:\n" ++ expected)


has : String -> String -> Expect.Expectation
has expected s =
    let
        result =
            Compiler.tree ("module MyModule exposing (nothing) \n" ++ s)
                |> Regex.replace All (regex "\\n( )+") (always "")
                |> Regex.replace All (regex "( )+") (always " ")
    in
        String.contains (String.trim expected) result
            |> Expect.true ("Code:\n" ++ result ++ "\n\ndoes not contain:\n" ++ expected)


tuples : Test
tuples =
    describe "Tuples"
        [ test "Tuples w spaces" <|
            \() ->
                "tuple = (1, 2)" |> has "{1, 2}"
        , test "Tuples w/o spaces" <|
            \() ->
                "tuple = ( 1, 2 )" |> has "{1, 2}"
        , test "Nested tuples" <|
            \() ->
                "tuple = (1, (2, 3))" |> has "{1, {2, 3}}"
        ]


lists : Test
lists =
    describe "Lists"
        [ test "Lists w spaces" <|
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
        , test "Cons operator" <|
            \() ->
                "list = 1 :: 2 :: [3]" |> has "[1 | [2 | [3]]]"
        ]


functions : Test
functions =
    describe "Functions"
        [ test "Application" <|
            \() ->
                "app = a b c d" |> has "a().(b()).(c()).(d())"
        , test "Uncurried application when all args provided" <|
            \() ->
                "a : a -> a -> a -> a\napp = a b c d" |> has "a(b(), c(), d())"
        , test "ffi" <|
            \() ->
                "upcase : String -> String\nupcase name = ffi \"String\" \"to_upper\" " |> has "String.to_upper("
        , test "Function names are snakecased" <|
            \() ->
                "camelCase = 1" |> has "camel_case()"
        , test "Function calls are snakecased" <|
            \() ->
                "a = camelCase 1" |> has "camel_case().(1)"
        , test "Uncurried function calls are snakecased" <|
            \() ->
                "fooBar : a -> a -> a\napp = fooBar 1 1" |> has "foo_bar(1, 2)"
        , test "Can call function recursively" <|
            \() ->
                "a = let f a = f (a - 1) in f" |> has "f = rec f, fn a ->"
        ]


binOps : Test
binOps =
    describe "Binary Operators"
        [ test "Simple ops" <|
            \() ->
                "add = a + b" |> has "a() + b()"
        , test "Ops as lambda" <|
            \() ->
                "add = (+)" |> has "(&+/0).()"
        , test "Ops as lambda with param" <|
            \() ->
                "add = ((+) 2)" |> has "(&+/0).().(2)"
        , test "Complex ops as lambda " <|
            \() ->
                "add = map (+) list" |> has "map().((&+/0).()).(list())"
        ]


specs : Test
specs =
    describe "Specs"
        [ test "Typespecs with dependant types" <|
            \() ->
                "sum : (List Int) -> Int" |> has "@spec sum(list(integer)) :: integer"
        , test "Typespecs with functions" <|
            \() ->
                "map : (List a) -> (a -> a) -> (List a)"
                    |> has "map(list(any), (any -> any)) :: list(any)"
        , test "Typespecs with functions #2" <|
            \() ->
                "map : (a -> a) -> (b -> b) -> (List a)"
                    |> has "map((any -> any), (any -> any)) :: list(any)"
        , test "Typespecs with multiple arg functions" <|
            \() ->
                "map : (List a) -> (a -> a -> b) -> (List a)"
                    |> has "map(list(any), (any -> (any -> any))) :: list(any) "
        , test "Typespecs names are snakecased" <|
            \() ->
                "mapMap : a" |> has "@spec map_map"
        , test "Records in typespecs" <|
            \() ->
                "record : { a : Int, b : String}" |> has "@spec record() :: %{a: integer,b: String.t}"
        , test "Remote typespecs" <|
            \() ->
                "f : Remote.Module.Type -> String.T"
                    |> has "f(Remote.Module.type) :: String.t"
        ]


records : Test
records =
    describe "Records"
        [ test "Records work" <|
            \() ->
                "a = { a = 1 }" |> has "%{a: 1}"
        , test "Complex records work" <|
            \() ->
                "a = { a = 1, b = 2, c = (a b)}" |> has "%{a: 1, b: 2, c: a().(b())}"
        , test "Updating records work" <|
            \() ->
                "addToA r = {r | a = (r.a + 5), b = 2} " |> has "%{r | a: (r.a + 5), b: 2}"
        ]


types : Test
types =
    describe "types"
        [ test "Types" <|
            \() ->
                "type AType = BType | CType" |> has "@type a_type :: :b_type | :c_type"
        , test "TypeRecord" <|
            \() ->
                "type alias A = {a : Int, b: Int, c: Int}"
                    |++ "a = A 1 2 3"
                    |> has "%{a: 1, b: 2, c: 3}"
        , test "Type alias application" <|
            \() ->
                "type alias A = {a : Int, b : Int}"
                    |++ "a = A 10"
                    |> has "fn arg1 -> %{a: 10, b: arg1} end"
        , test "Types work when applied incompletely" <|
            \() ->
                "type Focus = A Int Int | B Int Int Int"
                    |++ "a = B 1 1"
                    |> has "fn x1 -> {:b, 1, 1, x1} end"
        , test "TypeTuple" <|
            \() ->
                "type alias A = (Int, Int, Int)"
                    |++ "a = A 1 2 "
                    |> has "{arg1, arg2, arg3}"
        , test "Types ignore typealiases" <|
            \() ->
                "type alias AnyAlias = Lol"
                    |++ "type AnyType = AnyAlias | AnyType"
                    |> has "@type any_type :: :any_alias | :any_type"
        , test "Types can wrap records" <|
            \() ->
                "type Lens big small = Lens { get : big -> small }"
                    |> has "@type lens :: {:lens, %{get: (any -> any)}}"
        , test "Types args don't polute type application" <|
            \() ->
                "type Focus big small = Focus { get : big -> small }"
                    |++ "a = Focus { get = get, update = update }"
                    |> has "{:focus, %{get: get, update: update}}"
        ]


typeConstructors : Test
typeConstructors =
    describe "Type Constructors"
        [ test "Type application" <|
            \() ->
                "a = Type a b c" |> has "{:type, a(), b(), c()}"
        , test "Type in tuple" <|
            \() ->
                "a = (Type, a, b, c)" |> has "{:type, a(), b(), c()}"
        , test "Remote types" <|
            \() ->
                "a = Remote.Type a b c" |> has "{:type, a(), b(), c()}"
        , test "Remote types in tuples" <|
            \() ->
                "a = (Remote.Type, a, b, c)" |> has "{:type, a(), b(), c()}"
        ]


doctests : Test
doctests =
    describe "Doctests"
        [ test "Doctests" <|
            \() ->
                "{-| A equals 1. It just does\n"
                    ++ "what the hell\n"
                    ++ "    a == 1\n"
                    ++ "-}\n"
                    ++ "a = 1"
                    |> has "iex> a\n"
        ]


typeAliases : Test
typeAliases =
    describe "Type aliases in specs"
        [ test "TypeAlias substitution" <|
            \() ->
                "type alias MyType a = List a"
                    |++ "test : MyType Int"
                    |> has "@spec test() :: list("
        , test "Type substitution" <|
            \() ->
                "type MyType = Wende | NieWende"
                    |++ "test : MyType"
                    |> has "@spec test() :: my_type"
        , test "TypeAlias argument substitution" <|
            \() ->
                "type alias MyType a = List a"
                    |++ "test : MyType Int"
                    |> has "@spec test() :: list(integer)"
        , test "TypeAlias argument substitution between types" <|
            \() ->
                "type alias AnyKey val = (a, val)"
                    |++ "type alias Val a = AnyKey a"
                    |++ "test : Val Int"
                    |> has "@spec test() :: {any, integer}"
        , test "TypeAlias no argument substitution in Type" <|
            \() ->
                "type alias MyList a = List a"
                    |++ "type Val a = AnyKey (MyList a)"
                    |++ "test : Val Int"
                    |> has "@spec test() :: val"

        -- Polymorhpism
        , test "Polymorhpic record alias" <|
            \() ->
                "type Wende = Wende"
                    |++ "type alias Wendable a = { a | wendify : (a -> Wende)}"
                    |++ "type alias Man = Wendable { gender: Bool }"
                    |++ "a : Man -> String "
                    |> has "@spec a(%{wendify: (%{gender: boolean} -> wende), gender: boolean}) :: String.t"
        , test "Multi polymorhpic record alias" <|
            \() ->
                "type Wende = Wende"
                    |++ "type alias Namable a = { a | name : String }"
                    |++ "type alias Agable a =  { a | age: Int }"
                    |++ "type alias Man = Namable (Agable { gender : String })"
                    |++ "a : Man -> String "
                    |> has "@spec a(%{name: String.t, age: integer, gender: String.t}) :: String.t"
        , test "Interface as type" <|
            \() ->
                "type alias Namable a = { a | name : String }"
                    |++ "getName : Namable a -> String "
                    |> has "@spec get_name(%{name: String.t}) :: String.t"
        ]


fileImports =
    describe "Imports"
        [ test "Same alias names in two files" <|
            \() ->
                """
>>>> FileA.elm
module A exposing (..)
type alias A = Int

>>>> FileB.elm
module B exposing (..)
type alias B = Float
    """
                    -- If it compiles it's already good
                    |> hasFull ""
        , test "Imported alias from another file" <|
            \() ->
                """
>>>> FileA.elm
module A exposing (..)
type alias MyAlias = Int

>>>> FileB.elm
module B exposing (..)
import A exposing (..)

a : MyAlias
a = 1
    """
                    |> hasFull "@spec a() :: integer"
        , test "Imported type from another file" <|
            \() ->
                """
>>>> FileA.elm
module A exposing (..)
type MyType = TypeA Int | TypeB Int

a : MyType
a = TypeA

>>>> FileB.elm
module B exposing (..)
import A exposing (..)

a : MyType
a = TypeB
    """
                    |> hasFull "fn x1 -> {:type_b, x1} end"
        , test "Imported specific type from another file" <|
            \() ->
                """
>>>> FileA.elm
module A exposing (..)
type MyType = TypeA Int | TypeB Int

a : MyType
a = TypeA

>>>> FileB.elm
module B exposing (..)
import A exposing (MyType(TypeB))

a : MyType
a = TypeA
    """
                    |> hasFull ":type_a"
        , test "Imported all union types from another file" <|
            \() ->
                """
>>>> FileA.elm
module A exposing (..)
type MyType = TypeA Int | TypeB Int

a : MyType
a = TypeA

>>>> FileB.elm
module B exposing (..)
import A exposing (MyType(..))

a : MyType
a = (TypeA, TypeB)
    """
                    |> hasFull ":type_a"
        , test "Doesn't import what imports imported" <|
            \() ->
                """
>>>> A.elm
module A exposing (..)
type Invisible = Invi Int

>>>> B.elm
module B exposing (..)
import A exposing (..)

>>>> C.elm
module C exposing (..)
import A exposing (..)

>>>> B.elm
module D exposing (..)
import B exposing (..)
import C exposing (..)

a : Invisible
a = 1
    """
                    |> hasFull ":invisible"
        , test "Qualified imports work too" <|
            \() ->
                """
>>>> a.elm
module A exposing (A)
type As a = Tag a

>>>> b.elm
module B exposing (..)
import A exposing (As)
a : As a
a = Tag
       """
                    |> hasFull "fn x1 -> {:tag, x1} end"
        , test "Conflicted imports are excepts" <|
            \() ->
                """
>>>> a.elm
module Something.A exposing (a)
a : a -> Int
a _ = 1

>>>> b.elm
module Something.B exposing (..)
import Something.A exposing (..)
a : a -> Int
a _ = 10
       """
                    |> hasFull "import Something.A, except: [{:'a', 0}, {:'a', 1}]"
        ]


all : Test
all =
    describe "All"
        [ tuples
        , lists
        , functions
        , binOps

        -- Disabled util specs are working correctly
        , specs
        , typeAliases
        , types
        , records
        , typeConstructors
        , doctests
        , fileImports
        ]
