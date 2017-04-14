module Elmchemy exposing (..)


type alias Module =
    String


type alias Function =
    String


type alias Arity =
    Int


ffi : Module -> Function -> a
ffi =
    Debug.crash "You can't use ffi in browser"
