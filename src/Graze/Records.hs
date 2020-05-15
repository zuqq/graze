{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Records
    ( toSExpr
    ) where

import qualified Data.Text as T (Text)

import Graze.HttpUrl  (serialize)
import Graze.Messages (Record (..))

-- $setup
-- >>> :set -XOverloadedStrings


data SExpr
    = Leaf T.Text
    | Node [SExpr]

infixr 6 <+>

-- | Join two strings with a space, except if one of them is null.
--
-- ==== __Examples__
--
-- >>> "" <+> ""
-- ""
-- >>> "a" <+> ""
-- "a"
-- >>> "" <+> "a"
-- "a"
-- >>> "a" <+> "b"
-- "a b"
(<+>) :: T.Text -> T.Text -> T.Text
(<+>) "" s' = s'
(<+>) s ""  = s
(<+>) s s'  = s <> " " <> s'

-- | Append a newline character.
--
-- ==== __Examples__
--
-- >>> newline "a"
-- "a\n"
-- >>> newline ""
-- "\n"
newline :: T.Text -> T.Text
newline s = s <> "\n"

-- | Wrap the string in parentheses.
--
-- ==== __Examples__
--
-- >>> parens ""
-- "()"
-- >>> parens "a"
-- "(a)"
parens :: T.Text -> T.Text
parens s = "(" <> s <> ")"

-- | Wrap the string in double quotes.
--
-- ==== __Examples__
--
-- >>> doubleQuotes ""
-- "\"\""
-- >>> doubleQuotes "a"
-- "\"a\""
doubleQuotes :: T.Text -> T.Text
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
hsep :: [T.Text] -> T.Text
hsep = foldr (<+>) ""

-- | Pretty-print an S-expression.
--
-- Strings need to be quoted because URLs can contain parentheses.
--
-- ==== __Examples__
--
-- >>> pp $ Leaf "a"
-- "\"a\""
-- >>> pp $ Node [Leaf "a", Leaf "b"]
-- "(\"a\" \"b\")"
pp :: SExpr -> T.Text
pp (Leaf s)  = doubleQuotes s
pp (Node xs) = parens . hsep . fmap pp $ xs

-- | Encode a 'Record' as a newline-terminated S-expression.
toSExpr :: Record -> T.Text
toSExpr Record {..} = newline . pp $ Node
    [ Node [Leaf "url", Leaf url]
    , Node [Leaf "parent", Leaf parent]
    , Node [Leaf "children", Node children]
    ]
  where
    url      = serialize rUrl
    parent   = serialize rParent
    children = fmap (Leaf . serialize) rChildren
