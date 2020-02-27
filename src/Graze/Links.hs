{-# LANGUAGE OverloadedStrings #-}

module Graze.Links (links) where

import           Control.Monad         ((<=<))
import qualified Data.ByteString.Char8 as B (ByteString)
import           Data.Either           (rights)
import qualified Data.Text             as T

import Text.HTML.DOM   (parseBSChunks)
import Text.XML.Cursor (attribute, Cursor, descendant, element, fromDocument)

import Graze.HttpUrl        (HttpUrl(..))
import Graze.HttpUrl.Parser (parseRel)

-- $setup
-- >>> :set -XOverloadedStrings 


rawLinks :: Cursor -> [T.Text]
rawLinks = attribute "href" <=< element "a" <=< descendant

-- |
-- >>> straighten "/../"
-- "/"
-- >>> straighten "../a/"
-- "../a/"
-- >>> straighten "/a/b/.."
-- "/a"
-- >>> straighten "/a/b/../"
-- "/a/"
-- >>> straighten "/a/b/../c"
-- "/a/c"
-- >>> straighten "/a/b/./"
-- "/a/b/"
-- >>> straighten "/a/b/./c"
-- "/a/b/c"
-- >>> straighten "//"
-- "/"
-- >>> straighten "/a//b"
-- "/a/b"
straighten :: T.Text -> T.Text
straighten = T.replace "//" "/" . T.intercalate "/" . go . T.split (== '/')
  where
    go ("" : ".." : xs) = "" : go xs  -- For the special case "/.." -> "/".
    go (_ : ".." : xs)  = go xs       -- For the generic case "a/.." -> "a".
    go (x : "." : xs)   = x : go xs
    go (x : xs)         = x : go xs
    go xs               = xs

-- |
-- >>> stripFragment "http://a.b/c#d"
-- "http://a.b/c"
-- >>> stripFragment "http://a.b/c?d=e#f"
-- "http://a.b/c?d=e"
stripFragment :: T.Text -> T.Text
stripFragment = fst . T.breakOn "#"

links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = fmap clean
    . rights
    . fmap (parseRel base)
    . rawLinks
    . fromDocument
    . parseBSChunks
    . return
  where
    clean url = url { huPath = straighten . stripFragment . huPath $ url }
