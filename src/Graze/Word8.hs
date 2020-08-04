module Graze.Word8
    ( isAlpha
    , isNum
    , isPchar
    ) where

import Data.Word (Word8)


isAlpha :: Word8 -> Bool
isAlpha w = 65 <= w && w <= 90 || 97 <= w && w <= 122

isNum :: Word8 -> Bool
isNum w = 48 <= w && w <= 57

isSafe :: Word8 -> Bool
isSafe w = case w of
    36 -> True  -- '$'
    45 -> True  -- '-'
    95 -> True  -- '_'
    46 -> True  -- '.'
    43 -> True  -- '+'
    _  -> False

isExtra :: Word8 -> Bool
isExtra w = case w of
    33 -> True  -- '!'
    42 -> True  -- '*'
    39 -> True  -- '\''
    40 -> True  -- '('
    41 -> True  -- ')'
    44 -> True  -- ','
    _  -> False

isUchar :: Word8 -> Bool
isUchar w
    | w == 37   = True  -- '%'
    | isAlpha w = True
    | isNum w   = True
    | isSafe w  = True
    | isExtra w = True
    | otherwise = False

isPchar :: Word8 -> Bool
isPchar w
    | w == 47   = True  -- '/'
    | isUchar w = True
    | otherwise = case w of
        58 -> True  -- ':'
        64 -> True  -- '@'
        38 -> True  -- '&'
        61 -> True  -- '='
        _  -> False
