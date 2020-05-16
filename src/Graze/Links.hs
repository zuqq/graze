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
    ( Text
    , breakOn
    , filter
    , intercalate
    , replace
    , split
    )
import qualified Data.Text.Encoding as T (decodeUtf8)

import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

import Graze.HttpUrl (HttpUrl (..), parseRel)

-- $setup
-- >>> :set -XOverloadedStrings


rawLinks :: B.ByteString -> [T.Text]
rawLinks s = T.filter (not . isSpace) . T.decodeUtf8 <$>
    mapMaybe (lookup "href") [ as | TagOpen "a" as <- parseTags s ]

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
    go [""] (".." : ys)     = go [""] ys      -- If the input starts with "/../".
    go (_ : xs) (".." : ys) = go xs ys
    go xs (y : ys)          = go (y : xs) ys
    go xs []                = reverse xs      -- Base case.

normalize :: HttpUrl -> HttpUrl
normalize url = url {huPath = straighten . stripFragment . huPath $ url}

links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = S.toList
    . S.fromList
    . fmap normalize
    . rights
    . fmap (parseRel base)
    . rawLinks
