module UnitTests exposing (..)

import Test exposing (..)
import Expect


all : Test
all =
    describe "Test"
        [ test "Test truth" <|
            \() ->
                Expect.equal "a" "a"
        ]
