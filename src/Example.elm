module Stack exposing (..)


type alias TestRecord =
    { int : Int
    , float : Float
    }


blah : TestRecord
blah =
    42


blorp =
    blah.x
