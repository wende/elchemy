module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Compiler


all : Test
all =
    describe "Code compilation"
        [ test "simple definition" <|
            \() ->
                Expect.atLeast
                    1
                    (String.length
                        (Compiler.tree "module MyModule exposing (..)")
                    )
        ]
