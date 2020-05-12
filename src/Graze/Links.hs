{-# LANGUAGE OverloadedStrings #-}

module Graze.Links
    ( links
    ) where

import           Control.Monad   ((<=<))
import qualified Data.ByteString as B (ByteString)
import           Data.Char       (isSpace)
import           Data.Either     (rights)
import qualified Data.Text       as T

import Text.HTML.DOM   (parseBSChunks)
import Text.XML.Cursor (attribute, descendant, element, fromDocument)

import Graze.HttpUrl (HttpUrl (..), parseRel)

-- $setup
-- >>> :set -XOverloadedStrings


stripSpaces :: T.Text -> T.Text
stripSpaces = T.filter (not . isSpace)

rawLinks :: B.ByteString -> [T.Text]
rawLinks = fmap stripSpaces
    . (attribute "href" <=< element "a" <=< descendant)
    . fromDocument
    . parseBSChunks
    . return

-- | Strip the fragment from a URL.
--
-- ==== __Examples__
--
-- >>> stripFragment "http://a.b/c#d"
-- "http://a.b/c"
-- >>> stripFragment "http://a.b/c?d=e#f"
-- "http://a.b/c?d=e"
stripFragment :: T.Text -> T.Text
stripFragment = fst . T.breakOn "#"

-- | Remove relative elements from a path.
--
-- ==== __Examples__
--
-- >>> straighten "a"
-- "a"
-- >>> straighten "//"
-- "/"
-- >>> straighten "/a/b/./"
-- "/a/b/"
-- >>> straighten "/../"
-- "/"
-- >>> straighten "/a/b/../"
-- "/a/"
-- >>> straighten "/a/b/../../"
-- "/"
-- >>> straighten "/a/../../c"
-- "/c"
straighten :: T.Text -> T.Text
straighten = T.intercalate "/" . go [] . T.split (== '/') . T.replace "//" "/"
  where
    go xs ("." : ys)        = go xs ys
    go [""] (".." : ys)     = go [""] ys      -- If the input starts with "/../"
    go (_ : xs) (".." : ys) = go xs ys
    go xs (y : ys)          = go (y : xs) ys
    go xs []                = reverse xs      -- Base case

normalize :: HttpUrl -> HttpUrl
normalize url = url { huPath = straighten . stripFragment . huPath $ url }

links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = fmap normalize
    . rights
    . fmap (parseRel base)
    . rawLinks
