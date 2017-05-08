module Elmchemy exposing (..)


type alias Pid =
    ( Int, Int, Int )


type alias Module =
    String


type alias Function =
    String


type alias Arity =
    Int


lffi : Function -> a
lffi =
    Debug.crash "You can't call local ffi in a browser"


ffi : Module -> Function -> a
ffi m f =
    Debug.crash "You can't use ffi in a browser"

flambda : Int -> a -> b
flambda arity f =
    Debug.crash "You can't use foreign lambda in a browser"

super : a
super =
    Debug.crash "You can't use super in a browser"
