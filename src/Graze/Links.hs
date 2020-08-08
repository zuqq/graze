{-# LANGUAGE OverloadedStrings #-}

module Graze.Links
    ( links
    ) where

import qualified Data.ByteString          as B (ByteString)
import           Data.Char                (isSpace)
import           Data.Either              (rights)
import qualified Data.HashSet             as HS (fromList, toList)
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T (Text, filter)
import qualified Data.Text.Encoding       as T (decodeUtf8With)
import qualified Data.Text.Encoding.Error as T (lenientDecode)

import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

import Graze.HttpUrl (HttpUrl (..), parseRel)


hrefs :: T.Text -> [T.Text]
hrefs s = T.filter (not . isSpace) <$>
    mapMaybe (lookup "href") [ as | TagOpen "a" as <- parseTags s ]

-- | The expression @links base html@ is a list of the URLs of all links in the
-- HTML document @html@, with @base@ serving as the base URL for relative links.
links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = HS.toList
    . HS.fromList
    . rights
    . fmap (parseRel base)
    . hrefs
    . T.decodeUtf8With T.lenientDecode
