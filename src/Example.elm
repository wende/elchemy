module Stack exposing (..)

import Elmchemy exposing (..)


meta =
    [ "use GenServer" ]


type alias State a =
    List a


type GenServerReturn a b
    = Reply a (State b)
    | NoReply (State b)


type Command a
    = Stack
    | Push a
    | Pop



-- Client


startLink : a -> Pid
startLink default =
    ffi "GenServer" "start_link" ( Stack, default )


push : Pid -> a -> a
push pid item =
    ffi "GenServer" "cast" ( pid, (Push item) )


pop : Pid -> a
pop pid =
    ffi "GenServer" "call" ( pid, Pop )



-- Server (callbacks)


handle_call : Command a -> Pid -> State a -> GenServerReturn a a
handle_call command from state =
    case ( command, from, state ) of
        ( Pop, _, h :: t ) ->
            Reply h t

        ( request, from, state ) ->
            lffi "super" ( request, from, state )


handle_cast : Command a -> State a -> GenServerReturn a a
handle_cast command state =
    case ( command, state ) of
        ( Push item, state ) ->
            NoReply (item :: state)

        ( request, state ) ->
            lffi "super" ( request, state )
