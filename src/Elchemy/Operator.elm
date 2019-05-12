module Elchemy.Operator exposing (elixirBinop)

import Ast.Expression exposing (Expression(..))
import Elchemy.Context as Context exposing (Context, Parser)
import Elchemy.Elixir as Elixir exposing (EAst(..))
import Elchemy.Helpers as Helpers exposing (Operator(..), ind, operatorType, translateOperator)


{-| Encode binary operator inlcuding the researved ones
-}
elixirBinop : Context -> Parser -> String -> Expression -> Expression -> EAst
elixirBinop c elixirE op l r =
    case op of
        "//" ->
            EApp (EVar "div") [ ECheated <| elixirE c l, ECheated <| elixirE c r ]

        "%" ->
            EApp (EVar "rem") [ ECheated <| elixirE c l, ECheated <| elixirE c r ]

        "^" ->
            EApp (EAccess (EAtom "math") (EVar "")) [ ECheated <| elixirE c l, ECheated <| elixirE c r ]

        "::" ->
            ECons [ ECheated <| elixirE c l ] (ECheated <| elixirE c r)

        "<<" ->
            elixirBinop c elixirE ">>" r l

        "<|" ->
            if l == Variable [ "Do" ] then
                EQuoteBlock (ECheated <| elixirE c r)
            else
                elixirBinop c elixirE "|>" r l

        "|>" ->
            ECheated <|
                "("
                    ++ elixirE c l
                    ++ (flattenPipes r
                            |> List.map (elixirE c)
                            |> List.map ((++) (ind c.indent ++ "|> ("))
                            |> List.map (flip (++) ").()")
                            |> String.join ""
                       )
                    ++ ")"

        "as" ->
            EOp "=" (ECheated <| elixirE c l) (ECheated <| elixirE c r)

        op ->
            ECheated <|
                case operatorType op of
                    Builtin ->
                        [ "(", elixirE c l, " ", translateOperator op, " ", elixirE c r, ")" ]
                            |> String.join ""

                    Custom ->
                        translateOperator op
                            ++ "("
                            ++ elixirE c l
                            ++ ", "
                            ++ elixirE c r
                            ++ ")"

                    None ->
                        Context.crash c ("Illegal operator " ++ op)


{-| Flattens pipes into a list of expressions
-}
flattenPipes : Expression -> List Expression
flattenPipes e =
    case e of
        BinOp (Variable [ "|>" ]) l ((BinOp (Variable [ "|>" ]) r _) as n) ->
            [ l ] ++ flattenPipes n

        BinOp (Variable [ "|>" ]) l r ->
            [ l ] ++ [ r ]

        other ->
            [ other ]
