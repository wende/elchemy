module ExContext
    exposing
        ( Alias
        , Aliases
        , AliasType(..)
        , Arity
        , Context
        , Definition
        , Parser
        , empty
        , addAlias
        , getAlias
        , wrongArityAlias
        , addType
        , getType
        , indent
        , deindent
        , mergeVariables
        , inArgs
        , onlyWithoutFlag
        , hasFlag
        , addFlag
        , isPrivate
        , mergeTypes
        )

import Ast.Expression exposing (Expression)
import Ast.Statement exposing (ExportSet, Type(..), Statement, ExportSet(..))
import Dict exposing (Dict)
import Set exposing (Set)


{-| Alias representing function that produces code out of Context and Ast
-}
type alias Parser =
    Context -> Expression -> String


{-| A structure containing all the essential information about Type Alias
-}
type alias Alias =
    { mod : String
    , arity : Arity
    , aliasType : AliasType
    , body : Type
    , getTypeBody : List Type -> Type
    }


{-| Amount of the arguments a funtction or type takes
-}
type alias Arity =
    Int


{-| Dict holding sets of aliases indexed by module name and alias name
-}
type alias Aliases =
    Dict String (Dict String Alias)


{-| Dict holding sets of aliases indexed by module name and type name
-}
type alias Types =
    Dict String (Dict String Arity)


{-| Type of an Alias which can be either Type or Type Alias. It's important to note
that a Type in here is only a definition of a type like

    type A
        = TagA
        | TagB

Where only A is a `ExContext.AliasType.Type`, two separate tags are other instances and
belong to Types not Aliases

-}
type AliasType
    = Type
    | TypeAlias


{-| A flag for a compiler and its value
-}
type alias Flag =
    ( String, String )


{-| Definition of a function and its correspoint Ast.Type structure
-}
type alias Definition =
    { arity : Arity, def : Ast.Statement.Type }


{-| Context containing all the necessary information about current place in a file
like what's the name of a module, what aliases, types and variables are currently defined,
what flags were set for the compiler, what functions were defined and if it is in definition mode.
-}
type alias Context =
    { mod : String
    , exports : ExportSet
    , indent : Int
    , aliases : Aliases
    , types : Types
    , flags : List Flag
    , definitions : Dict String Definition
    , variables : Set String
    , inArgs : Bool
    , hasModuleDoc : Bool
    , lastDoc : Maybe String
    }


{-| Crashes the compiler because the alias was used with wrong arity.
Shouldn't ever happen if run after elm-make
-}
wrongArityAlias : Int -> List Type -> String -> a
wrongArityAlias arity list name =
    "Expected "
        ++ toString arity
        ++ " arguments for "
        ++ name
        ++ ". But got "
        ++ (toString <| List.length list)
        |> Debug.crash


{-| Adds an alias definition to the context
-}
addAlias : String -> String -> Alias -> Context -> Context
addAlias mod name ali context =
    let
        putAlias maybeMod =
            maybeMod
                |> Maybe.map (Dict.insert name ali)
                |> Maybe.withDefault (Dict.singleton name ali)
                |> Just
    in
        { context | aliases = Dict.update mod putAlias context.aliases }


{-| Adds a type definition to the context
-}
addType : String -> String -> Int -> Context -> Context
addType mod name arity context =
    let
        putType maybeMod =
            maybeMod
                |> Maybe.map (Dict.insert name arity)
                |> Maybe.withDefault (Dict.singleton name arity)
                |> Just
    in
        { context | types = Dict.update mod putType context.types }


{-| Get's either alias or type from context based on `from` accessor
-}
getFromContext :
    (Context -> Dict String (Dict String a))
    -> String
    -> String
    -> Context
    -> Maybe a
getFromContext from mod name context =
    context
        |> from
        |> Dict.get mod
        |> Maybe.andThen (Dict.get name)


{-| Get's an alias from context based on name of a module and of an alias
Wrapped in Maybe
-}
getAlias : String -> String -> Context -> Maybe Alias
getAlias =
    getFromContext .aliases


{-| Get's a type from context based on name of a module and of a type
Wrapped in Maybe
-}
getType : String -> String -> Context -> Maybe Arity
getType =
    getFromContext .types



--{-|
--Merges types and aliases of a given module name with current context module
---}
-- importFromContext :
--     -> String
--     -> String
--     -> Context
--     -> Context
-- importFromContext mod name context =
--     context
--         |> from
--         |> Dict.get mod


{-| Returns empty context
-}
empty : String -> ExportSet -> Context
empty name exports =
    Context name
        exports
        0
        Dict.empty
        Dict.empty
        []
        Dict.empty
        Set.empty
        False
        False
        Nothing


{-| Increases current indenation level of a context
-}
indent : Context -> Context
indent c =
    { c | indent = c.indent + 1 }


{-| Decreases current indenation level of a context
-}
deindent : Context -> Context
deindent c =
    { c | indent = c.indent - 1 }


{-| Adds a flag to the compiler of a context
-}
addFlag : Flag -> Context -> Context
addFlag flag c =
    { c | flags = flag :: c.flags }


{-| Puts the code only if the given flag and its given value DOESN'T exist
-}
onlyWithoutFlag : Context -> String -> String -> String -> String
onlyWithoutFlag c key value code =
    if hasFlag key value c then
        ""
    else
        code


{-| -}
getAllFlags : String -> Context -> List String
getAllFlags key c =
    c.flags
        |> List.filter (Tuple.first >> ((==) key))
        |> List.map Tuple.second


{-| True if has a flag with a particular value
-}
hasFlag : String -> String -> Context -> Bool
hasFlag key value c =
    c.flags
        |> List.any ((==) ( key, value ))


{-| Makes the state to be inside argument declaration,
thanks to that the compiler knows not to treat the declaration of new variables
as a refference to an older values or functions and prevents injection of parens
-}
inArgs : Context -> Context
inArgs c =
    { c | inArgs = True }


{-| Tells you if a function is private or public based on context of a module
-}
isPrivate : Context -> String -> Bool
isPrivate context name =
    case context.exports of
        SubsetExport exports ->
            if List.any (\exp -> exp == FunctionExport name) exports then
                False
            else
                True

        AllExport ->
            False

        other ->
            Debug.crash "No such export"


{-| Merges a set of two variables from two different contexts
-}
mergeVariables : Context -> Context -> Context
mergeVariables left right =
    { left | variables = Set.union left.variables right.variables }


{-| Merges everything that should be imported from given module, based
on given export set value
-}
mergeTypes : ExportSet -> String -> Context -> Context
mergeTypes set mod c =
    let
        getAll name dict =
            Dict.get name dict
                |> Maybe.withDefault Dict.empty

        getThese name f dict =
            getAll name dict
                |> Dict.filter (\key _ -> (f key))

        importConflict : String -> v -> v -> Dict String v -> Dict String v
        importConflict key a b _ =
            Debug.crash
                ("You can't have two same imports for name "
                    ++ key
                    ++ "\nFirst one is:\n"
                    ++ toString a
                    ++ "\n Second one is:\n"
                    ++ toString b
                )

        mergeDicts : Dict String v -> Dict String v -> Dict String v
        mergeDicts left right =
            Dict.merge Dict.insert importConflict Dict.insert left right Dict.empty

        addThese : String -> Dict String v -> Dict String (Dict String v) -> Dict String (Dict String v)
        addThese name add dict =
            Dict.update
                name
                (\a ->
                    Just <|
                        case a of
                            Just current ->
                                mergeDicts add current

                            Nothing ->
                                add
                )
                dict

        getTypeNames : Maybe ExportSet -> List String
        getTypeNames subset =
            case subset of
                Just (SubsetExport list) ->
                    list
                        |> List.map
                            (\a ->
                                case a of
                                    FunctionExport name ->
                                        name

                                    _ ->
                                        Debug.crash
                                            ("Something went wrong with "
                                                ++ toString a
                                            )
                            )

                Nothing ->
                    []

                _ ->
                    Debug.crash ("Something went wrong with " ++ toString subset)
    in
        case set of
            AllExport ->
                { c
                    | types = addThese c.mod (getAll mod c.types) c.types
                    , aliases = addThese c.mod (getAll mod c.aliases) c.aliases
                }

            SubsetExport list ->
                list
                    |> List.foldl
                        (\a c ->
                            case a of
                                TypeExport aliasName types ->
                                    { c
                                        | aliases =
                                            (addThese
                                                c.mod
                                                (getThese mod ((==) aliasName) c.aliases)
                                                c.aliases
                                            )
                                        , types =
                                            (addThese c.mod
                                                (getThese mod
                                                    (flip List.member <| (Debug.log "Types" getTypeNames) types)
                                                    c.types
                                                )
                                                c.types
                                            )
                                    }

                                FunctionExport _ ->
                                    c

                                _ ->
                                    Debug.crash "You can't import subset of subsets"
                        )
                        c

            _ ->
                Debug.crash "You can't import something that's not a subset"
