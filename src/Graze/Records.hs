{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Records
    ( toSExpr
    ) where

import qualified Data.Text as T (Text)

import Graze.HttpUrl  (serialize)
import Graze.Messages (PageRecord (..))


data SExpr
    = Leaf T.Text
    | Node [SExpr]

infixr 6 <+>

-- |
--
-- ==== __Examples__
(<+>) :: T.Text -> T.Text -> T.Text
(<+>) "" s' = s'
(<+>) s ""  = s
(<+>) s s'  = s <> " " <> s'

-- |
--
-- ==== __Examples__
newline :: T.Text -> T.Text
newline s = s <> "\n"

-- |
--
-- ==== __Examples__
parens :: T.Text -> T.Text
parens s = "(" <> s <> ")"

-- |
--
-- ==== __Examples__
doubleQuotes :: T.Text -> T.Text
doubleQuotes s = "\"" <> s <> "\""

-- |
--
-- ==== __Examples__
hsep :: [T.Text] -> T.Text
hsep = foldr (<+>) ""

-- | Pretty-print an S-expression.
--
-- Strings need to be quoted because URLs can contain parentheses.
pp :: SExpr -> T.Text
pp (Leaf s)  = doubleQuotes s
pp (Node xs) = parens . hsep . fmap pp $ xs

-- | Encode a 'PageRecord' as an S-expression.
--
-- ==== __Examples__
toSExpr :: PageRecord -> T.Text
toSExpr PageRecord {..} = newline . pp $ Node
    [ Node [Leaf "url", Leaf url]
    , Node [Leaf "parent", Leaf parent]
    , Node [Leaf "children", Node children]
    ]
  where
    url      = serialize prUrl
    parent   = serialize prParent
    children = fmap (Leaf . serialize) prChildren
