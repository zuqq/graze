{-# LANGUAGE OverloadedStrings #-}

module Graze.Links (links) where

import           Control.Monad         ((<=<))
import qualified Data.ByteString.Char8 as B (ByteString)
import           Data.Either           (rights)
import qualified Data.Text             as T

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
-- >>> straighten "/../"
-- "/"
-- >>> straighten "/a/b/../"
-- "/a/"
-- >>> straighten "/a/b/./"
-- "/a/b/"
-- >>> straighten "//"
-- "/"
-- >>> straighten "a"
-- "a"
straighten :: T.Text -> T.Text
straighten = T.intercalate "/" . go . T.split (== '/') . T.replace "//" "/"
  where
    go ("." : xs)       = go xs
    go (".." : xs)      = go xs
    go ("" : ".." : xs) = "" : go xs  -- For the special case "/../" -> "/".
    go (_ : ".." : xs)  = go xs
    go (x : xs)         = x : go xs
    go xs               = xs

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
