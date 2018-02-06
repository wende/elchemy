module Elchemy.XDict
    exposing
        ( Dict
        , empty
        , singleton
        , insert
        , update
        , isEmpty
        , get
        , remove
        , member
        , size
        , filter
        , partition
        , foldl
        , foldr
        , map
        , union
        , intersect
        , diff
        , merge
        , keys
        , values
        , toList
        , fromList
        )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import Elchemy exposing (..)


{- flag notype:+Dict -}
{- ex
   @type dict(_, _) :: %{}
-}


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Dict


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    ffi "Native.Dict" "empty"


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    get "Tom"   (fromList [ ("Tom", Cat), ("Jerry", Mouse) ]) == Just Cat
    get "Jerry" (fromList [ ("Tom", Cat), ("Jerry", Mouse) ]) == Just Mouse
    get "Spike" (fromList [ ("Tom", Cat), ("Jerry", Mouse) ]) == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get targetKey dict =
    ffi "Native.Dict" "get"


{-| Determine if a key is in a dictionary.
-}
member : comparable -> Dict comparable v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.

    size (fromList [("a", 1), ("b", 2)]) == 2

-}
size : Dict k v -> Int
size dict =
    ffi "Native.Dict" "size"


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    dict == empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.

    insert "a" 1 empty == fromList [("a", 1)] == True

-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value dict =
    ffi "Native.Dict" "insert"


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.

    isEmpty (remove "a" (fromList [("a", 1)])) == True

-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
    ffi "Native.Dict" "remove"


{-| Update the value of a dictionary for a specific key with a given function.

    update "a" (\_ -> Just 2) (fromList [("a", 1)]) == (fromList [("a", 2)]) == True

-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update k alter dict =
    ffi "Native.Dict" "update"


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    insert key value empty



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable v -> Dict comparable v -> Dict comparable v
diff t1 t2 =
    foldl (\k _ t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

    merge insert (flip (always insert)) insert (fromList [("a", 1), ("b", 2)]) (fromList [("b", 3), ("c", 4)]) empty == (fromList [("a", 1), ("b", 3), ("c", 4)]) == True

-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    ffi "Native.Dict" "merge"



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (comparable -> a -> b) -> Dict comparable a -> Dict comparable b
map f dict =
    ffi "Native.Dict" "map"


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldl f acc dict =
    ffi "Native.Dict" "foldl"


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldr f acc t =
    ffi "Native.Dict" "foldr"


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter predicate dictionary =
    let
        add key value dict =
            if predicate key value then
                insert key value dict
            else
                dict
    in
        foldl add empty dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
partition predicate dict =
    let
        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )
    in
        foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : Dict comparable v -> List comparable
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : Dict comparable v -> List v
values dict =
    foldr (\_ value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict comparable v -> List ( comparable, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs
