module Elchemy.XDebug
    exposing
        ( log
        , crash
        )

{-| Module with helper functions for debugging


# Debug

@docs log, crash

-}

import Elchemy exposing (..)


type Device
    = Stdio


{-| Log to console in `title: object` format

     log "Title" (1,2,3) == (1,2,3)

-}
log : String -> a -> a
log title a =
    let
        _ =
            puts_ Stdio "#{title}: #{inspect a}"
    in
        a


puts_ : Device -> a -> a
puts_ =
    ffi "IO" "puts"



{- We don't verify since it's a macro -}
{- flag noverify:+crash -}


{-| Raise an exception to crash the runtime. Should be avoided at all
costs. Helpful for crashing at not yet implelented functionality
-}
crash : String -> a
crash =
    ffi "Kernel" "raise"
