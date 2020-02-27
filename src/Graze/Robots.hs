module Graze.Robots (disallowedBy, rules, Rules) where

import qualified Data.Text as T

import Graze.HttpUrl      (HttpUrl(..))
import Graze.Robots.Parser (parse)
import Graze.Robots.Trie   (fromList, member, Trie)


type Chunks = [T.Text]

type Rules = Trie T.Text

-- | Split at @'/'@, dropping any empty substrings.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedStrings
-- >>> chunk "a"
-- ["a"]
-- >>> chunk "a/"
-- ["a"]
-- >>> chunk "a/b"
-- ["a","b"]
chunk :: T.Text -> Chunks
chunk = filter (not . T.null) . T.split (== '/')

rules :: T.Text -> Rules
rules = fromList . fmap chunk . parse

disallowedBy :: HttpUrl -> Rules -> Bool
disallowedBy = member . chunk . path
