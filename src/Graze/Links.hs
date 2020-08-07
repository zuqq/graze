{-# LANGUAGE OverloadedStrings #-}

module Graze.Links
    ( links
    ) where

import           Data.Char             (isSpace)
import           Data.Either           (rights)
import qualified Data.HashSet          as HS (fromList, toList)
import           Data.Maybe            (mapMaybe)
import qualified Data.ByteString.Char8 as C (ByteString, filter)

import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

import Graze.HttpUrl (HttpUrl (..), parseRel)


hrefs :: C.ByteString -> [C.ByteString]
hrefs s = C.filter (not . isSpace) <$>
    mapMaybe (lookup "href") [ as | TagOpen "a" as <- parseTags s ]

-- | The expression @links base html@ is a list of the URLs of all links in the
-- HTML document @html@, with @base@ serving as the base URL for relative links.
links :: HttpUrl -> C.ByteString -> [HttpUrl]
links base = HS.toList
    . HS.fromList
    . rights
    . fmap (parseRel base)
    . hrefs
