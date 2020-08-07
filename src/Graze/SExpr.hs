{-# LANGUAGE OverloadedStrings #-}

module Graze.SExpr
    ( SExpr (..)
    , toByteString
    ) where

import           Data.ByteString.Builder
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L

-- $setup
-- >>> :set -XOverloadedStrings


-- | An S-expression is a tree in which leaves are labeled with values of type
-- 'B.ByteString' and interior nodes carry no label.
data SExpr = Leaf !B.ByteString | Node ![SExpr]

toBuilder :: SExpr -> Builder
toBuilder (Leaf s)        = charUtf8 '"' <> byteString s <> charUtf8 '"'
toBuilder (Node [])       = stringUtf8 "()"
toBuilder (Node (x : xs)) = charUtf8 '('
    <> toBuilder x
    <> mconcat [charUtf8 ' ' <> toBuilder x' | x' <- xs]
    <> charUtf8 ')'

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
toByteString :: SExpr -> L.ByteString
toByteString = toLazyByteString . toBuilder
