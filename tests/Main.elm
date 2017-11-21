port module Main exposing (..)

import Test
import Tests
import UnitTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit (Test.describe "Elchemy" [ Tests.all, UnitTests.all ])


port emit : ( String, Value ) -> Cmd msg
