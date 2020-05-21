{-# LANGUAGE OverloadedStrings #-}

module Graze.SExpr
    ( SExpr (..)
    , toByteString
    ) where

import qualified Data.ByteString as B (ByteString, intercalate)

-- $setup
-- >>> :set -XOverloadedStrings


data SExpr = Leaf B.ByteString | Node [SExpr]

-- | Wrap the string in parentheses.
--
-- ==== __Examples__
--
-- >>> parens ""
-- "()"
-- >>> parens "a"
-- "(a)"
parens :: B.ByteString -> B.ByteString
parens s = "(" <> s <> ")"

-- | Wrap the string in double quotes.
--
-- ==== __Examples__
--
-- >>> doubleQuotes ""
-- "\"\""
-- >>> doubleQuotes "a"
-- "\"a\""
doubleQuotes :: B.ByteString -> B.ByteString
doubleQuotes s = "\"" <> s <> "\""

-- | Join the elements of the list with a space.
--
-- ==== __Examples__
--
-- >>> hsep []
-- ""
-- >>> hsep [""]
-- ""
-- >>> hsep ["a"]
-- "a"
-- >>> hsep ["a", "b"]
-- "a b"
hsep :: [B.ByteString] -> B.ByteString
hsep = B.intercalate " "

toByteString :: SExpr -> B.ByteString
toByteString (Leaf s)  = doubleQuotes s
toByteString (Node xs) = parens . hsep . fmap toByteString $ xs
