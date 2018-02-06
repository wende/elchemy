module Elchemy.Plugins.ExUnit exposing (..)

{-| a
-}

import Elchemy.Elixir.Plugin


plugin =
    Plugin.Plugin
        { setup = [ "use ExUnit.Case" ]
        , serialize = serialize
        }
