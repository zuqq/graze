{-# LANGUAGE OverloadedStrings #-}

module Graze.Links (links) where

import           Control.Monad   ((<=<))
import qualified Data.ByteString as B (ByteString)
import           Data.Either     (rights)
import qualified Data.Text       as T

import Text.HTML.DOM   (parseBSChunks)
import Text.XML.Cursor (attribute, Cursor, descendant, element, fromDocument)

import Graze.HttpUrl        (HttpUrl (..))
import Graze.HttpUrl.Parser (parseRel)

-- $setup
-- >>> :set -XOverloadedStrings 


rawLinks :: Cursor -> [T.Text]
rawLinks = attribute "href" <=< element "a" <=< descendant

-- |
-- >>> stripFragment "http://a.b/c#d"
-- "http://a.b/c"
-- >>> stripFragment "http://a.b/c?d=e#f"
-- "http://a.b/c?d=e"
stripFragment :: T.Text -> T.Text
stripFragment = fst . T.breakOn "#"

-- |
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
    go (x : xs) (".." : ys) = go xs ys
    go xs (y : ys)          = go (y : xs) ys
    go xs []                = reverse xs      -- Base case

clean :: HttpUrl -> HttpUrl
clean url = url { huPath = straighten . stripFragment . huPath $ url }

links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = fmap clean
    . rights
    . fmap (parseRel base)
    . rawLinks
    . fromDocument
    . parseBSChunks
    . return
