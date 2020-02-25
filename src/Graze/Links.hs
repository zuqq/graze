{-# LANGUAGE OverloadedStrings #-}

module Graze.Links (links) where

import           Control.Monad              ((<=<))
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.Text.Lazy             as TL

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (attribute, Cursor, descendant, element, fromDocument)

import Graze.Http (HttpUrl(..), folder)

-- $setup
-- >>> :set -XOverloadedStrings 


rawLinks :: Cursor -> [TL.Text]
rawLinks = fmap TL.fromStrict . (attribute "href" <=< element "a" <=< descendant)

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
normalize :: TL.Text -> TL.Text
normalize = TL.replace "//" "/" . TL.intercalate "/" . go . TL.split (== '/')
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
stripFragment :: TL.Text -> TL.Text
stripFragment = fst . TL.breakOn "#"

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
absolute :: HttpUrl -> TL.Text -> HttpUrl
absolute url l = HttpUrl s d (normalize . stripFragment $ p)
  where
    (s, (d, p))
        | Just l' <- TL.stripPrefix "//" l       = (scheme url, TL.breakOn "/" l')
        | Just l' <- TL.stripPrefix "http://" l  = ("http", TL.breakOn "/" l')
        | Just l' <- TL.stripPrefix "https://" l = ("https", TL.breakOn "/" l')
        | "/" `TL.isPrefixOf` l                  = (scheme url, (domain url, l))
        | otherwise = (scheme url, (domain url, folder url <> l))

-- | Given an 'HttpUrl' @url@ and a 'BL.ByteString' @bs@ representing an HTML
-- document, map @absolute url@ over the links in @bs@ and return the result.
links :: HttpUrl -> BL.ByteString -> [HttpUrl]
links url = fmap (absolute url) . rawLinks . fromDocument . parseLBS
