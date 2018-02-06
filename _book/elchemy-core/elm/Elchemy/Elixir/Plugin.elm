module Elchemy.Elixir.Plugin exposing (Plugin)

{-| Used to allow usage of macros inside Elchemy
-}

{- ex

   @spec encode_module(String.t) :: String.t
   def encode_module(name) do
     hash_id = bin2 |> :erlang.crc32 |> Integer.to_string(32)
     :"Elixir.#{name}.H#{hash_id}"
   end

   def defplugin() do
   end

-}


type alias Plugin a =
    { setup : List String
    , blocks : List a
    , serialize : a -> String
    }


plugin =
    3
