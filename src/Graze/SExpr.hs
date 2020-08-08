module Graze.SExpr
    ( SExpr (..)
    , toLazyText
    ) where

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

-- $setup
-- >>> :set -XOverloadedStrings


-- | An S-expression is a tree in which leaves are labeled with values of type
-- 'B.ByteString' and interior nodes carry no label.
data SExpr = Leaf !T.Text | Node ![SExpr]

parens :: TLB.Builder -> TLB.Builder
parens b = TLB.singleton '(' <> b <> TLB.singleton ')'

doubleQuotes :: TLB.Builder -> TLB.Builder
doubleQuotes b = TLB.singleton '"' <> b <> TLB.singleton '"'

toBuilder :: SExpr -> TLB.Builder
toBuilder (Leaf s)  = doubleQuotes $ TLB.fromText s
toBuilder (Node xs) = parens $ case xs of
    []        -> mempty
    (x : xs') -> toBuilder x
        <> mconcat [TLB.singleton ' ' <> toBuilder x' | x' <- xs']

-- | Serialize an S-expression.
--
-- ==== __Examples__
--
-- >>> toLazyText $ Leaf ""
-- "\"\""
-- >>> toLazyText $ Leaf "a"
-- "\"a\""
-- >>> toLazyText $ Node [Leaf "a", Leaf "b"]
-- "(\"a\" \"b\")"
-- >>> toLazyText $ Node [Leaf "a", Node [Leaf "b", Leaf "c"]]
-- "(\"a\" (\"b\" \"c\"))"
-- >>> toLazyText $ Node [Leaf "a", Node [Leaf "b", Node [Leaf "c"]]]
-- "(\"a\" (\"b\" (\"c\")))"
toLazyText :: SExpr -> TL.Text
toLazyText = TLB.toLazyText . toBuilder
