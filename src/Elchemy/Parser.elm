module Elchemy.Parser exposing (Parser(..))

import Elchemy.Elixir as Elixir exposing (EAst(..))


type Parser context input
    = Parser (context -> input -> Result String EAst)


parse : Parser context input -> context -> input -> Result String EAst
parse (Parser p) =
    p


return : (context -> input -> Result String EAst) -> Parser context input
return f =
    Parser f



--(<$>) : (EAst -> a) -> (Result a) -> context -> input -> Result String a
--(<$>) f c
--(<*>) :


type Test
    = Test String


parserTest c x =
    Ok x


test =
    Test <$> parserTest {} 10
