module Elchemy.XBitwise
    exposing
        ( and
        , or
        , xor
        , complement
        , shiftLeftBy
        , shiftRightBy
        , shiftRightZfBy
        )

{-| Library for [bitwise operations](http://en.wikipedia.org/wiki/Bitwise_operation).


# Basic Operations

@docs and, or, xor, complement


# Bit Shifts

@docs shiftLeftBy, shiftRightBy, shiftRightZfBy

-}

{- ex
   use Bitwise
-}

import Elchemy exposing (..)


integerBitSize =
    32


{-| Bitwise AND

    and 0 0 == 0
    and 1 1 == 1
    and 4 1 == 0
    and 102939 1 == 1
    -- truncates to 32 bits
    and 1099511627775 -1 == -1

-}
and : Int -> Int -> Int
and arg1 arg2 =
    to32Bits (and_ arg1 arg2)



{- We don't verify since it's a macro -}
{- flag noverify:+and_ -}


and_ : Int -> Int -> Int
and_ =
    ffi "Bitwise" "band"


{-| Bitwise OR

    or 0 0 == 0
    or 1 1 == 1
    or 4 1 == 5
    or 102939 1 == 102939
    -- truncates to 32 bits
    or 1099511627775 0 == -1

-}
or : Int -> Int -> Int
or arg1 arg2 =
    to32Bits (or_ arg1 arg2)



{- We don't verify since it's a macro -}
{- flag noverify:+or_ -}


or_ : Int -> Int -> Int
or_ =
    ffi "Bitwise" "bor"


{-| Bitwise XOR

    xor 0 0 == 0
    xor 1 1 == 0
    xor 4 1 == 5
    xor 102939 1 == 102938
    -- truncates to 32 bits
    xor 1099511627775 1 == -2

-}
xor : Int -> Int -> Int
xor arg1 arg2 =
    to32Bits (xor_ arg1 arg2)



{- We don't verify since it's a macro -}
{- flag noverify:+xor_ -}


xor_ : Int -> Int -> Int
xor_ =
    ffi "Bitwise" "bxor"


{-| Flip each bit individually, often called bitwise NOT

    complement 0 == -1
    complement 1 == -2
    complement 102939 == -102940
    -- truncates to 32 bits
    complement 1099511627775 == 0

-}
complement : Int -> Int
complement arg =
    to32Bits (complement_ arg)



{- We don't verify since it's a macro -}
{- flag noverify:+complement_ -}


complement_ : Int -> Int
complement_ =
    ffi "Bitwise" "bnot"


{-| Shift bits to the left by a given offset, filling new bits with zeros.
This can be used to multiply numbers by powers of two.

    shiftLeftBy 1 5 == 10
    shiftLeftBy 5 1 == 32
    -- shift is modulo 32 bits
    shiftLeftBy 32 1 == 1
    -- truncates to 32 bits
    shiftLeftBy 16 65535 == -65536

-}
shiftLeftBy : Int -> Int -> Int
shiftLeftBy shift int =
    to32Bits (shiftLeftBy_ int (mod_ shift integerBitSize))



{- We don't verify since it's a macro -}
{- flag noverify:+shiftLeftBy_ -}


shiftLeftBy_ : Int -> Int -> Int
shiftLeftBy_ =
    ffi "Bitwise" "bsl"


mod_ : Int -> Int -> Int
mod_ =
    ffi "Integer" "mod"


{-| Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This can be used to divide numbers by powers of two.

    shiftRightBy 1  32 == 16
    shiftRightBy 2  32 == 8
    shiftRightBy 1 -32 == -16
    -- shift is modulo 32 bits
    shiftRightBy 32 1 == 1
    -- truncates to 32 bits
    shiftRightBy 1 1099511627775 == -1

This is called an [arithmetic right shift][ars], often written (>>), and
sometimes called a sign-propagating right shift because it fills empty spots
with copies of the highest bit.
[ars]: <http://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift>

-}
shiftRightBy : Int -> Int -> Int
shiftRightBy shift int =
    to32Bits (shiftRightBy_ int (mod_ shift integerBitSize))



{- We don't verify since it's a macro -}
{- flag noverify:+shiftRightBy_ -}


shiftRightBy_ : Int -> Int -> Int
shiftRightBy_ =
    ffi "Bitwise" "bsr"


{-| Shift bits to the right by a given offset, filling new bits with zeros.

    shiftRightZfBy 1  32 == 16
    shiftRightZfBy 2  32 == 8
    shiftRightZfBy 1 -32 == 2147483632
    -- shift is modulo 32 bits
    shiftRightZfBy 32 1 == 1
    -- truncates to 32 bits
    shiftRightZfBy 1 1099511627775 == 2147483647

This is called an [logical right shift][lrs], often written (>>>), and
sometimes called a zero-fill right shift because it fills empty spots with
zeros.
[lrs]: <http://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift>

-}
shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy shift int =
    shiftRightBy shift int
        |> and (zfMask (mod_ shift integerBitSize))


zfMask : Int -> Int
zfMask bits =
    (shiftLeftBy_ 1 (integerBitSize - bits)) - 1



{- flag noverify:+to32Bits -}


to32Bits : Int -> Int
to32Bits int =
    ffi "Elchemy.XBitwise" "to_32_bits_"



{- ex

   def to_32_bits_(int) do
     << truncated :: integer-signed-32 >> = << int :: integer-signed-32 >>
     truncated
   end

-}
