module Elchemy.Context
    exposing
        ( Alias
        , AliasType(..)
        , Commons
        , Context
        , FunctionDefinition
        , Module
        , Parser
        , TypeBody(..)
        , addAlias
        , addFlag
        , addFunctionDefinition
        , addModuleAlias
        , addType
        , areMatchingArity
        , changeCurrentModule
        , crash
        , deindent
        , empty
        , emptyCommons
        , getAlias
        , getArity
        , getShadowedFunctions
        , getType
        , hasFlag
        , importBasicsWithoutShadowed
        , inArgs
        , indent
        , isPrivate
        , listOfImports
        , maybeModuleAlias
        , mergeTypes
        , mergeVariables
        , notImplemented
        , onlyWithoutFlag
        , putIntoModule
        , wrongArityAlias
        )

import Ast.Expression exposing (Expression)
import Ast.Statement exposing (ExportSet(..), Statement, Type(..))
import Dict exposing (Dict)
import Elchemy.Helpers as Helpers exposing (toSnakeCase, toSnakeCaseAtom)
import Set exposing (Set)


type alias Parser =
    Context -> Expression -> String


{-| A structure containing all the essential information about Type Alias
-}
type TypeBody
    = SimpleType Type
    | ArgumentedType String (List Type) Type


type alias Alias =
    { parentModule : String
    , arity : Int
    , aliasType : AliasType
    , body : Type
    , typeBody : TypeBody
    , types : List String
    }


type alias UnionType =
    { arity : Int
    , parentModule : String
    , parentAlias : String
    }


{-| Type of an Alias which can be either Type or Type Alias. It's important to note
that a Type in here is only a definition of a type like

    type A
        = TagA
        | TagB

Where only A is a `Context.AliasType.Type`, two separate tags are other instances and
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
type alias FunctionDefinition =
    { arity : Int, def : Ast.Statement.Type }


{-| Dict holding information about defined modules
-}
type alias Module =
    { aliases : Dict String Alias
    , types : Dict String UnionType
    , functions : Dict String FunctionDefinition
    , exports : ExportSet
    }


type alias Commons =
    { modules : Dict String Module
    }


{-| Context containing all the necessary information about current place in a file
like what's the name of a module, what aliases, types and variables are currently defined,
what flags were set for the compiler, what functions were defined and if it is in definition mode.
-}
type alias Context =
    { mod : String
    , commons : Commons
    , exports : ExportSet
    , indent : Int
    , flags : List Flag
    , variables : Set String
    , inArgs : Bool
    , hasModuleDoc : Bool
    , lastDoc : Maybe String
    , inTypeDefiniton : Bool
    , importedTypes : Dict String String
    , aliasedModules : Dict String String

    -- Dict functionName (moduleName, arity)
    , importedFunctions : Dict String ( String, Int )
    , meta : Maybe Expression
    , inMeta : Bool
    }


{-| Crashes the compiler because the alias was used with wrong arity.
Shouldn't ever happen if run after elm-make
-}
wrongArityAlias : Context -> Int -> List Type -> String -> a
wrongArityAlias c arity list name =
    crash c <|
        "Expected "
            ++ toString arity
            ++ " arguments for "
            ++ name
            ++ ". But got "
            ++ (toString <| List.length list)


{-| Puts something into module
Usage:

    putIntoModule "Module" "name" .aliases (x -> { c | aliases = x }) ali c

-}
putIntoModule :
    String
    -> String
    -> (Module -> Dict String a)
    -> (Module -> Dict String a -> Module)
    -> a
    -> Context
    -> Context
putIntoModule mod name getter setter thing c =
    let
        updateMod : Maybe Module -> Maybe Module
        updateMod maybeMod =
            maybeMod
                |> Maybe.map getter
                |> Maybe.withDefault Dict.empty
                |> Dict.update name (always <| Just thing)
                |> setter (maybeMod |> Maybe.withDefault emptyModule)
                |> Just

        commons =
            c.commons
    in
        { c | commons = { commons | modules = commons.modules |> Dict.update mod updateMod } }


{-| Adds an alias definition to the context
-}
addAlias : String -> String -> Alias -> Context -> Context
addAlias mod name =
    putIntoModule mod name .aliases (\m x -> { m | aliases = x })


{-| Adds a type definition to the context
-}
addType : String -> String -> String -> Int -> Context -> Context
addType mod parentAlias name arity =
    let
        t =
            { arity = arity, parentModule = mod, parentAlias = parentAlias }
    in
        putIntoModule mod name .types (\m x -> { m | types = x }) t


{-| Add type definition into context
-}
addFunctionDefinition : Context -> String -> FunctionDefinition -> Context
addFunctionDefinition c name d =
    putIntoModule c.mod name .functions (\m x -> { m | functions = x }) d c


{-| Get's either alias or type from context based on `from` accessor
-}
getFromContext :
    (Module -> Dict String a)
    -> String
    -> String
    -> Context
    -> Maybe a
getFromContext from mod name context =
    context
        |> .commons
        |> .modules
        |> Dict.get mod
        |> Maybe.map from
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
getType : String -> String -> Context -> Maybe UnionType
getType =
    getFromContext .types


{-| Gets arity of the function in the module
-}
getArity : Context -> String -> String -> Maybe Int
getArity ctx m fn =
    let
        local =
            ctx.commons.modules
                |> Dict.get m
                |> Maybe.map .functions
                |> Maybe.andThen (Dict.get fn)
                |> Maybe.map .arity

        imported =
            ctx.importedFunctions
                |> Dict.get fn
                |> Maybe.map Tuple.second
    in
        Helpers.maybeOr local imported


{-| Checks if function arity stored in context is the same as arguments count
-}
areMatchingArity : Context -> String -> String -> List a -> Bool
areMatchingArity c mod fn args =
    List.length args == Maybe.withDefault -1 (getArity c mod fn)


{-| Returns empty context
-}
empty : String -> ExportSet -> Context
empty name exports =
    { mod = name
    , exports = exports
    , indent = 0
    , flags = []
    , variables = Set.empty
    , inArgs = False
    , hasModuleDoc = False
    , lastDoc = Nothing
    , commons = { modules = Dict.singleton name (Module Dict.empty Dict.empty Dict.empty exports) }
    , inTypeDefiniton = False
    , importedTypes =
        Dict.fromList
            [ ( "Order", "Elchemy.XBasics" )
            , ( "Result", "Elchemy.XResult" )
            ]
    , importedFunctions = Dict.empty
    , aliasedModules = Dict.empty
    , meta = Nothing
    , inMeta = False
    }


{-| Returns empty commons structure
-}
emptyCommons : Commons
emptyCommons =
    { modules = Dict.empty
    }


changeCurrentModule : String -> Context -> Context
changeCurrentModule mod c =
    { c | mod = mod }


{-| Returns empty module record
-}
emptyModule : Module
emptyModule =
    Module Dict.empty Dict.empty Dict.empty AllExport


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
        |> List.filter (Tuple.first >> (==) key)
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
            if List.any ((==) (FunctionExport name)) exports then
                False
            else
                True

        AllExport ->
            False

        other ->
            crash context "No such export"


{-| Merges a set of two variables from two different contexts
-}
mergeVariables : Context -> Context -> Context
mergeVariables left right =
    { left | variables = Set.union left.variables right.variables }


{-| Finds all defined functions and all auto imported functions (XBasics) and returns
the commons subset. Return empty list for XBasics
-}
getShadowedFunctions : Context -> List String -> List ( String, FunctionDefinition )
getShadowedFunctions context list =
    let
        functions =
            context.commons.modules
                |> Dict.get context.mod
                |> Maybe.map .functions
                |> Maybe.withDefault Dict.empty

        findReserved name =
            functions
                |> Dict.get name
                |> Maybe.map ((,) name >> List.singleton)
                |> Maybe.withDefault []
    in
        if context.mod == "Elchemy.XBasics" then
            []
        else
            list
                |> List.concatMap findReserved


{-| Changes function definitions to a list of qualified imports including 0 and full arity
-}
listOfImports : List ( String, FunctionDefinition ) -> List String
listOfImports shadowed =
    let
        importTuple ( name, arity ) =
            toSnakeCaseAtom name
                ++ ": 0, "
                ++ toSnakeCaseAtom name
                ++ ": "
                ++ toString arity
    in
        shadowed
            |> List.map (Tuple.mapSecond .arity)
            |> List.map importTuple


{-| Get code representation of import XBasics with exclusion of functions defined locally
-}
importBasicsWithoutShadowed : Context -> String
importBasicsWithoutShadowed c =
    let
        importModule mod list =
            if list /= [] then
                list
                    |> String.join ", "
                    |> (++) ("import " ++ mod ++ ", except: [")
                    |> flip (++) "]\n"
            else
                ""

        shadowedBasics =
            getShadowedFunctions c Helpers.reservedBasicFunctions
                |> listOfImports

        shadowedKernel =
            getShadowedFunctions c Helpers.reservedKernelFunctions
                |> listOfImports
    in
        importModule "Elchemy.XBasics" shadowedBasics
            ++ importModule "Kernel" shadowedKernel


{-| Register a new module alias
import ModuleA as ModuleB
Would delias all ModuleB calls to ModuleA in case of Type and TypeAlias constructors
-}
addModuleAlias : String -> Maybe String -> Context -> Context
addModuleAlias oldName newName c =
    newName
        |> Maybe.map (\name -> { c | aliasedModules = c.aliasedModules |> Dict.insert name oldName })
        |> Maybe.withDefault c


{-| Replace a module name with it's original name it aliases to. Otherwise return the same name
-}
maybeModuleAlias : Context -> String -> String
maybeModuleAlias c s =
    c.aliasedModules
        |> Dict.get s
        |> Maybe.withDefault s


{-| Merges everything that should be imported from given module, based
on given export set value
-}
mergeTypes : ExportSet -> String -> Context -> Context
mergeTypes set mod c =
    let
        getAll getter mod =
            c.commons.modules
                |> Dict.get mod
                |> Maybe.map getter
                |> Maybe.withDefault Dict.empty

        getAlias : String -> Dict String Alias
        getAlias aliasName =
            getAll .aliases mod
                |> Dict.filter (\k _ -> k == aliasName)

        getTypes : String -> Maybe ExportSet -> Dict String UnionType
        getTypes aliasName maybeExportSet =
            getAll .types mod
                |> Dict.filter (\k { parentAlias } -> parentAlias == aliasName)

        putAllLocal getter setter dict c =
            Dict.foldl (\key value acc -> putIntoModule c.mod key getter setter value acc) c dict

        importOne export c =
            case export of
                TypeExport aliasName types ->
                    c
                        |> putAllLocal .aliases (\m x -> { m | aliases = x }) (getAlias aliasName)
                        |> putAllLocal .types (\m x -> { m | types = x }) (getTypes aliasName types)

                FunctionExport _ ->
                    c

                _ ->
                    crash c "You can't import subset of subsets"
    in
        case set of
            AllExport ->
                c
                    |> putAllLocal .aliases (\m x -> { m | aliases = x }) (getAll .aliases mod)
                    |> putAllLocal .types (\m x -> { m | types = x }) (getAll .types mod)

            SubsetExport list ->
                List.foldl importOne c list

            _ ->
                crash c "You can't import something that's not a subset"


{-| Throw a nice error with the context involving it
-}
crash : Context -> String -> a
crash c prompt =
    Debug.crash <|
        "Compilation error:\n\n\t"
            ++ prompt
            ++ "\n\nin module: "
            ++ c.mod


{-| Throw a nice error saying that this feature is not implemented yet
-}
notImplemented : Context -> String -> a -> String
notImplemented c feature value =
    " ## ERROR: No "
        ++ feature
        ++ " implementation for "
        ++ toString value
        ++ " yet"
        ++ "\n"
        |> Debug.crash
