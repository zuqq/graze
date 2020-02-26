module Graze.Robots (disallowedBy, empty, rules, Rules) where

import qualified Data.Text.Lazy as TL

import Graze.Http (HttpUrl(..))
import Graze.Robots.Parser (parse)
import Graze.Robots.Trie


type Chunks = [TL.Text]

type Rules = Trie TL.Text

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
chunk :: TL.Text -> Chunks
chunk = filter (not . TL.null) . TL.split (== '/')

rules :: TL.Text -> Rules
rules = fromList . fmap chunk . parse

disallowedBy :: HttpUrl -> Rules -> Bool
disallowedBy = member . chunk . path
