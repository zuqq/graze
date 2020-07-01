{-# LANGUAGE OverloadedStrings #-}

module Graze.SExpr
    ( SExpr (..)
    , toByteString
    ) where

import qualified Data.ByteString as B (ByteString, intercalate)

-- $setup
-- >>> :set -XOverloadedStrings


-- | An S-expression is a tree in which leaves are labeled with values of type
-- 'B.ByteString' and interior nodes carry no label.
data SExpr = Leaf !B.ByteString | Node ![SExpr]

parens :: B.ByteString -> B.ByteString
parens s = "(" <> s <> ")"

doubleQuotes :: B.ByteString -> B.ByteString
doubleQuotes s = "\"" <> s <> "\""

hsep :: [B.ByteString] -> B.ByteString
hsep = B.intercalate " "

-- | Serialize an S-expression.
--
-- ==== __Examples__
--
-- >>> toByteString $ Leaf ""
-- "\"\""
-- >>> toByteString $ Leaf "a"
-- "\"a\""
-- >>> toByteString $ Node [Leaf "a", Leaf "b"]
-- "(\"a\" \"b\")"
-- >>> toByteString $ Node [Leaf "a", Node [Leaf "b", Leaf "c"]]
-- "(\"a\" (\"b\" \"c\"))"
-- >>> toByteString $ Node [Leaf "a", Node [Leaf "b", Node [Leaf "c"]]]
-- "(\"a\" (\"b\" (\"c\")))"
toByteString :: SExpr -> B.ByteString
toByteString (Leaf s)  = doubleQuotes s
toByteString (Node xs) = parens . hsep . fmap toByteString $ xs
