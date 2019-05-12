module Elchemy.Elixir exposing (EAst(..), cheat, toString)


type EAst
    = EVar String
    | EApp EAst (List EAst)
    | EOp String EAst EAst
    | ELambdaApp EAst (List EAst)
    | ELambdify Int EAst
    | EAccess EAst EAst
    | ELambda (List EAst) EAst
    | EAtom String
    | EModule String
    | ETuple (List EAst)
    | ECheated String
    | EMap (List ( EAst, EAst ))
    | EMapUpdate EAst (List ( EAst, EAst ))
    | EQuoteBlock EAst
    | EList (List EAst)
    | ECons (List EAst) EAst



-- Here start to String mappings


cheat : EAst -> String
cheat =
    toString


toString : EAst -> String
toString ast =
    case ast of
        EApp exp args ->
            toString exp ++ "(" ++ (List.map toString args |> String.join ",") ++ ")"

        ELambda args body ->
            "fn " ++ (List.map toString args |> joinWithComma) ++ " -> " ++ toString body ++ "end"

        ELambdaApp exp args ->
            toString exp ++ ".(" ++ (List.map toString args |> String.join ", ") ++ ")"

        EVar name ->
            name

        EAtom "nil" ->
            "nil"

        EAtom "true" ->
            ":true"

        EAtom "false" ->
            ":false"

        EAtom name ->
            ":" ++ name

        ECheated string ->
            string

        EAccess left right ->
            toString left ++ "." ++ toString right

        ELambdify arity ast ->
            "(&" ++ toString ast ++ "/" ++ Basics.toString arity ++ ")"

        EModule mod ->
            mod

        EMapUpdate ast keyValuePairs ->
            "%{" ++ toString ast ++ " | " ++ (keyValuePairs |> toKeywords |> joinWithComma) ++ "}"

        EList elements ->
            "[" ++ (List.map toString elements |> joinWithComma) ++ "]"

        ECons elements target ->
            "[" ++ (List.map toString elements |> joinWithComma) ++ " | " ++ toString target ++ "]"

        EOp op left right ->
            "(" ++ toString left ++ " " ++ op ++ " " ++ toString right ++ ")"

        EQuoteBlock block ->
            "quote do " ++ toString block ++ " end"

        _ ->
            Debug.crash "Translation to AST not implemented yet"


toKeywords : List ( EAst, EAst ) -> List String
toKeywords list =
    List.map (\( name, val ) -> toString name ++ ": " ++ toString val) list


joinWithComma : List String -> String
joinWithComma =
    String.join ", "
