{-# LANGUAGE OverloadedStrings #-}

module Graze.Links (links) where

import           Control.Monad         ((<=<))
import qualified Data.ByteString.Char8 as B (ByteString)
import qualified Data.Text             as T

import Text.HTML.DOM   (parseBSChunks)
import Text.XML.Cursor (attribute, Cursor, descendant, element, fromDocument)

import Graze.Http (HttpUrl(..), folder)

-- $setup
-- >>> :set -XOverloadedStrings 


rawLinks :: Cursor -> [T.Text]
rawLinks = attribute "href" <=< element "a" <=< descendant

-- | Convert paths with relative components ".." and "." to absolute paths
-- and replace every occurency of "//" by "/".
--
-- ==== __Examples__
--
-- >>> normalize "/../"
-- "/"
-- >>> normalize "../a/"
-- "../a/"
-- >>> normalize "/a/b/.."
-- "/a"
-- >>> normalize "/a/b/../"
-- "/a/"
-- >>> normalize "/a/b/../c"
-- "/a/c"
-- >>> normalize "/a/b/./"
-- "/a/b/"
-- >>> normalize "/a/b/./c"
-- "/a/b/c"
-- >>> normalize "//"
-- "/"
-- >>> normalize "/a//b"
-- "/a/b"
normalize :: T.Text -> T.Text
normalize = T.replace "//" "/" . T.intercalate "/" . go . T.split (== '/')
  where
    go ("" : ".." : xs) = "" : go xs  -- For the special case "/.." -> "/".
    go (_ : ".." : xs)  = go xs       -- For the generic case "a/.." -> "a".
    go (x : "." : xs)   = x : go xs
    go (x : xs)         = x : go xs
    go xs               = xs

-- | Remove the fragment part of a link.
--
-- ==== __Examples__
--
-- >>> stripFragment "http://a.b/c#d"
-- "http://a.b/c"
-- >>> stripFragment "http://a.b/c?d=e#f"
-- "http://a.b/c?d=e"
stripFragment :: T.Text -> T.Text
stripFragment = fst . T.breakOn "#"

-- | Given an 'HttpUrl' @url@ and a link @l@, return the 'HttpUrl' that @l@
-- would correspond to if it were found on an HTML page at @url@. Note that this
-- function yields garbage if the link is invalid or its scheme is not HTTP(S).
--
-- ==== __Examples__
--
-- >>> let url = HttpUrl "http" "x" "/y/"
-- >>> absolute url "//a/b"
-- http://a/b
-- >>> absolute url "http://a/b"
-- http://a/b
-- >>> absolute url "https://a/b"
-- https://a/b
-- >>> absolute url "/a/b"
-- http://x/a/b
-- >>> absolute url "a/b"
-- http://x/y/a/b
-- >>> let url = HttpUrl "https" "x" "/y/"
-- >>> absolute url "a/b"
-- https://x/y/a/b
absolute :: HttpUrl -> T.Text -> HttpUrl
absolute url l = HttpUrl s d (normalize . stripFragment $ p)
  where
    (s, (d, p))
        | Just l' <- T.stripPrefix "//" l       = (scheme url, T.breakOn "/" l')
        | Just l' <- T.stripPrefix "http://" l  = ("http", T.breakOn "/" l')
        | Just l' <- T.stripPrefix "https://" l = ("https", T.breakOn "/" l')
        | "/" `T.isPrefixOf` l                  = (scheme url, (domain url, l))
        | otherwise = (scheme url, (domain url, folder url <> l))

-- | Given an 'HttpUrl' @url@ and a 'B.ByteString' @bs@ representing an HTML
-- document, map @absolute url@ over the links in @bs@ and return the result.
links :: HttpUrl -> B.ByteString -> [HttpUrl]
links url = fmap (absolute url) . rawLinks . fromDocument . parseBSChunks . return
