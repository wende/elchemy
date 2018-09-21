module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Elchemy.Compiler as Compiler
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
            Compiler.tree ("module MyModule exposing (nothing) \n{-| Moduledoc -}" ++ s)
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
    let
        testModules =
            """
>>>> b.elm
module B exposing (..)

import A exposing (..)

testFull : Int
testFull = A.fun 1 2

testCurried : Int
testCurried = A.fun 1

join : List String -> String
join a = String.join " " a

tested : Float
tested = importedFun 10 10.0

>>>> a.elm
module A exposing (fun, importedFun)
fun : Int -> Int -> Int
fun a b = 1

importedFun : Int -> Float -> Float
importedFun a b = 1
"""
    in
        describe "Functions"
            [ test "Application" <|
                \() ->
                    "app = a b c d" |> has "a().(b()).(c()).(d())"
            , test "Uncurried application when all args provided" <|
                \() ->
                    "a : a -> a -> a -> a"
                        |++ "app = a b c d"
                        |> has "a(b(), c(), d())"
            , test "ffi" <|
                \() ->
                    "upcase : String -> String"
                        |++ "upcase name = ffi \"String\" \"to_upper\" "
                        |> has "String.to_upper("
            , test "macro" <|
                \() ->
                    "upcase : Macro"
                        |++ "upcase = macro \"String\" \"to_upper\" "
                        |> has "String.to_upper("
            , test "macro doesn't lambdify arguments" <|
                \() ->
                    "upcase : (Int -> Int) -> Macro"
                        |++ "upcase = macro \"String\" \"to_upper\" "
                        |> has "String.to_upper(a1)"
            , test "Function names are snakecased" <|
                \() ->
                    "camelCase = 1" |> has "camel_case()"
            , test "Function calls are snakecased" <|
                \() ->
                    "a = camelCase 1" |> has "camel_case().(1)"
            , test "Uncurried function calls are snakecased" <|
                \() ->
                    "fooBar : a -> a -> a"
                        |++ "app = fooBar 1 2"
                        |> has "foo_bar(1, 2)"
            , test "Can call function recursively" <|
                \() ->
                    "a = let f a = f (a - 1) in f"
                        |> has "f = rec f, fn a ->"
            , test "Correct curried application from modules" <|
                \() -> testModules |> hasFull "A.fun().(1)"
            , test "Correct full application from modules" <|
                \() -> testModules |> hasFull "A.fun(1, 2)"
            , test "Correct curried application for undefined module" <|
                \() -> testModules |> hasFull "Elchemy.XString.join().(\" \").(a)"
            , test "Correct curried application for imported functions" <|
                \() -> testModules |> hasFull "imported_fun(10, 10.0)"
            ]


binOps : Test
binOps =
    describe "Binary Operators"
        [ test "Simple ops" <|
            \() ->
                "add = a + b" |> has "a() + b()"
        , test "Ops as lambda" <|
            \() ->
                "add = (+)" |> has "(&XBasics.+/0).()"
        , test "Ops as lambda with param" <|
            \() ->
                "add = ((+) 2)" |> has "(&XBasics.+/0).().(2)"
        , test "Complex ops as lambda " <|
            \() ->
                "add = map (+) list" |> has "map().((&XBasics.+/0).()).(list())"
        ]


specs : Test
specs =
    describe "Specs"
        [ test "Typespecs with dependant types" <|
            \() ->
                "sum : (List Int) -> Int"
                    |++ "sum = 0"
                    |> has "@spec sum(list(integer)) :: integer"
        , test "Typespecs with functions" <|
            \() ->
                "map : (List a) -> (a -> a) -> (List a)"
                    |++ "map = 0"
                    |> has "map(list(any), (any -> any)) :: list(any)"
        , test "Typespecs with functions #2" <|
            \() ->
                "map : (a -> a) -> (b -> b) -> (List a)"
                    |++ "map = 0"
                    |> has "map((any -> any), (any -> any)) :: list(any)"
        , test "Typespecs with multiple arg functions" <|
            \() ->
                "map : (List a) -> (a -> a -> b) -> (List a)"
                    |++ "map = 0"
                    |> has "map(list(any), (any -> (any -> any))) :: list(any) "
        , test "Typespecs names are snakecased" <|
            \() ->
                "mapMap : a"
                    |++ "mapMap = 0"
                    |> has "@spec map_map"
        , test "Records in typespecs" <|
            \() ->
                "record : { a : Int, b : String}"
                    |++ "record = 0"
                    |> has "@spec record() :: %{a: integer,b: String.t}"
        , test "Remote typespecs" <|
            \() ->
                "f : Remote.Module.Type -> String.T"
                    |++ "f = 0"
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
                    |> has "@type lens(big, small) :: {:lens, %{get: (big -> small)}}"
        , test "Types args don't polute type application" <|
            \() ->
                "type Focus big small = Focus { get : big -> small }"
                    |++ "a = Focus { get = get, update = update }"
                    |> has "{:focus, %{get: get(), update: update()}}"
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
                    ++ "    a == 1\n"
                    ++ "-}\n"
                    ++ "a : Int\n"
                    ++ "a = 1\n"
                    |> has "iex> a\n"
        ]


typeAliases : Test
typeAliases =
    describe "Type aliases in specs"
        [ test "TypeAlias substitution" <|
            \() ->
                "type alias MyType a = List a"
                    |++ "test : MyType Int"
                    |++ "test = 0"
                    |> has "@spec test() :: my_type(integer"
        , test "Type substitution" <|
            \() ->
                "type MyType = Wende | NieWende"
                    |++ "test : MyType"
                    |++ "test = 0"
                    |> has "@spec test() :: my_type"
        , test "TypeAlias argument substitution" <|
            \() ->
                "type alias MyType a = List a"
                    |++ "test : MyType Int"
                    |++ "test = 0"
                    |> has "@spec test() :: my_type(integer)"
        , test "TypeAlias argument substitution between types" <|
            \() ->
                "type alias AnyKey val = (a, val)"
                    |++ "type alias Val a = AnyKey a"
                    |++ "test : Val Int"
                    |++ "test = 0"
                    |> has "@spec test() :: val(integer)"
        , test "TypeAlias no argument substitution in Type" <|
            \() ->
                "type alias MyList a = List a"
                    |++ "type Val a = AnyKey (MyList a)"
                    |++ "test : Val Int"
                    |++ "test = 0"
                    |> has "@spec test() :: val"

        -- Polymorhpism
        , test "Polymorhpic record alias" <|
            \() ->
                "type Wende = Wende"
                    |++ "type alias Wendable a = { a | wendify : (a -> Wende)}"
                    |++ "type alias Man = Wendable { gender: Bool }"
                    |++ "a : Man -> String "
                    |++ "a = 0"
                    |> has "@type man :: %{wendify: (%{gender: boolean} -> wende), gender: boolean"
        , test "Multi polymorhpic record alias" <|
            \() ->
                "type Wende = Wende"
                    |++ "type alias Namable a = { a | name : String }"
                    |++ "type alias Agable a =  { a | age: Int }"
                    |++ "type alias Man = Namable (Agable { gender : String })"
                    |++ "a : Man -> String "
                    |++ "a = 0"
                    |> has "@type man :: %{name: String.t, age: integer, gender: String.t}"
        , test "Interface as type" <|
            \() ->
                "type alias Namable a = { a | name : String }"
                    |++ "getName : Namable a -> String"
                    |++ "getName = 0"
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
                    |> hasFull "@spec a() :: A.my_alias"
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
        , test "Named type from another aliased module" <|
            \() ->
                """
>>>> Foo.elm
module Foo exposing (Baz)
type alias Baz = {a: Int, b: Int}

>>>> Bar.elm
module Bar exposing (..)

import Foo

a : Foo.Baz
a = Foo.Baz 10 20
            """
                    |> hasFull "%{a: 10, b: 20}"
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
                    |> hasFull "invisible"
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


letIns : Test
letIns =
    describe "Let in constructs"
        [ test "Allows to reffer variables in reversed order" <|
            \() -> """
            test = let
              a = b
              b = 2
            in a
            """ |> has "b = 2a = b"
        , test "More advanced order" <|
            \() -> """
              test = let
                a = b
                b = 2
                c = a
                d = a + c
              in d
                """ |> has "b = 2a = bc = ad = (a + c)"
        , test "Functions work too" <|
            \() -> """
              test = let
                a = \\() -> b
                b = 10
              in d
                """ |> has "b = 10a = fn {} -> b end"
        , test "Union types aren't functions" <|
            \() -> """
              test = let
                A a = A 1
                b = 10
              in d
                """ |> has "{:a, a} = {:a, 1}b = 10"
        , test "Sugared functions work too (with arguments)" <|
            \() -> """
              test = let
                a x = x + b
                b = 10
                x = 1
              in x
                """ |> has "b = 10a = rec a, fn x -> (x + b) endx = 1xend"
        , test "Multiple sugared functions work too" <|
            \() -> """
              test = let
                a x = x + b
                b a = a
                x = 1
              in x
                """ |> has "b = rec b, fn a -> a enda = rec a, fn x -> (x + b) endx = 1xend"
        , test "Doesn't mind shadowing" <|
            \() -> """
              test = let
                a = \\b -> b
                b = 10
              in d
                """ |> has "a = fn b -> b endb = 10"
        , test "Doesn't mind destructuring" <|
            \() -> """
                test = let
                  newX = x + 1
                  (x, y) = (1, 2)
                  newY = y + 1
                in newX
                  """ |> has "{x, y} = {1, 2}new_x = (x + 1)new_y = (y + 1)"

        -- , test "Solves simple mutual recursion" <|
        --     \() -> """
        --       test =
        --         let
        --           fx a = fy + 1
        --           fy b = fx + 1
        --         in fx 10
        --           """ |> has "{fx, fy} = let [fx: fn a -> "
        -- , test "Solves mutual recursion" <|
        --     \() -> """
        --       test =
        --         let
        --           fx x = case x of
        --               0 -> 0
        --               x -> fy x - 1
        --           end
        --           fy x = case x of
        --               0 -> 0
        --               x -> fx x - 1
        --           end
        --         in fx 10
        --           """ |> has "{fx, fy} = let [fx: fn x -> "
        -- , test "Solves mutual relation" <|
        --     \() -> """
        --       test =
        --         let
        --           x = y + 1
        --           y = x + 1
        --         in x
        --           """ |> has "{x, y} = let [x: (y + 1)"
        ]


caseOfs =
    describe "Case ofs"
        [ test "Simple case of" <|
            \() ->
                """
test =
  case x of
    1 -> 1
    2 -> 2
                """ |> has "case x() do1 ->12 ->2end"
        , test "Nested case of" <|
            \() ->
                """
test =
  case x of
    1 -> 1
    2 -> case y of
      0 -> 0
    3 -> 3

            """ |> has "case x() do1 ->12 ->case y() do0 ->0end3 ->3end"
        ]


accessMacros : Test
accessMacros =
    describe "Access macros compile properly"
        [ test "Update"
            (\() ->
                "test = updateIn .a (\\a -> a + 1)" |> has "update_in_([:a]).(fn a -> (a + 1) end)"
            )
        , test "Update5"
            (\() ->
                "test = updateIn5 .a .b .c .d .e (\\a -> a + 1) v" |> has "update_in_([:a, :b, :c, :d, :e]).(fn a -> (a + 1) end).(v())"
            )
        , test "Get"
            (\() ->
                "test = getIn3 .something .something .darkSide True v" |> has "get_in_([:something, :something, :dark_side]).(:true).(v())"
            )
        , test "Put"
            (\() ->
                "test = putIn4 .a .b .c .d 10 v" |> has "put_in_([:a, :b, :c, :d]).(10).(v())"
            )
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
        , letIns
        , caseOfs
        , accessMacros
        ]
