module Elmchemy.XChar exposing
  ( isUpper, isLower, isDigit, isOctDigit, isHexDigit
  , toUpper, toLower
  , KeyCode, toCode, fromCode
  )

{-| Functions for working with characters. Character literals are enclosed in
`'a'` pair of single quotes.

# Classification
@docs isUpper, isLower, isDigit, isOctDigit, isHexDigit

# Conversion
@docs toUpper, toLower, toLocaleUpper, toLocaleLower

# Key Codes
@docs KeyCode, toCode, fromCode

-}
import Elmchemy exposing (..)

{-| True for char between first and second argument
    between 'a' 'z' 'b' == True
    between '1' '9' '5' == True
    between 'A' 'Z' 'g'  == False
 -}
isBetween : Char -> Char -> Char -> Bool
isBetween low high char =
  let code = toCode char
  in
      (code >= toCode low) && (code <= toCode high)


{-| True for upper case ASCII letters.
    isUpper 'D' == True
    isUpper 'A' == True
    isUpper 'x' == False
 -}
isUpper : Char -> Bool
isUpper char =
  isBetween 'A' 'Z' char


{-| True for lower case ASCII letters.
    isLower 'd' == True
    isLower 'a' == True
    isLower 'X' == False
 -}
isLower : Char -> Bool
isLower char =
  isBetween 'a' 'z' char


{-| True for ASCII digits `[0-9]`.
    isDigit '1' == True
    isDigit '9' == True
    isDigit 'a' == False
 -}
isDigit : Char -> Bool
isDigit char =
  isBetween '0' '9' char


{-| True for ASCII octal digits `[0-7]`.
    isOctDigit '7' == True
    isOctDigit '5' == True
    isOctDigit '9' == False
 -}
isOctDigit : Char -> Bool
isOctDigit char =
  isBetween '0' '7' char


{-| True for ASCII hexadecimal digits `[0-9a-fA-F]`.
    isHexDigit 'd' == True
    isHexDigit 'D' == True
    isHexDigit 'x' == False
 -}
isHexDigit : Char -> Bool
isHexDigit char =
  isDigit char || isBetween 'a' 'f' char || isBetween 'A' 'F' char


{-| Convert to upper case.
    toUpper 'a' == 'A'
 -}
toUpper : Char -> Char
toUpper char =
    ffi ":string" "to_upper" char


{-| Convert to lower case.
    toLower 'A' == 'a'
 -}
toLower : Char -> Char
toLower char =
    ffi ":string" "to_lower" char


-- {-| Convert to upper case, according to any locale-specific case mappings. -}
-- toLocaleUpper : Char -> Char
-- toLocaleUpper =
--   Native.Char.toLocaleUpper


-- {-| Convert to lower case, according to any locale-specific case mappings. -}
-- toLocaleLower : Char -> Char
-- toLocaleLower =
--   Native.Char.toLocaleLower


{-| Keyboard keys can be represented as integers. These are called *key codes*.
You can use [`toCode`](#toCode) and [`fromCode`](#fromCode) to convert between
key codes and characters.
-}
type alias KeyCode = Int


{-| Convert to key code.
    toCode 'a' == 97
-}
toCode : Char -> KeyCode
toCode char =
  lffi "hd" char


{-| Convert from key code.
    fromCode 97 == 'a'
 -}
fromCode : KeyCode -> Char
fromCode code =
    ffi "List" "insert_at" ([], 0, code)
