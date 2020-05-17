{-# LANGUAGE OverloadedStrings #-}

module Graze.Links
    ( links
    ) where

import qualified Data.ByteString    as B (ByteString)
import           Data.Char          (isSpace)
import           Data.Either        (rights)
import           Data.Maybe         (mapMaybe)
import qualified Data.Set           as S (fromList, toList)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T (decodeUtf8)

import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

import Graze.HttpUrl (HttpUrl (..), parseRel)

-- $setup
-- >>> :set -XOverloadedStrings


rawLinks :: B.ByteString -> [T.Text]
rawLinks s = T.filter (not . isSpace) . T.decodeUtf8 <$>
    mapMaybe (lookup "href") [ as | TagOpen "a" as <- parseTags s ]

links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = S.toList
    . S.fromList
    . rights
    . fmap (parseRel base)
    . rawLinks
