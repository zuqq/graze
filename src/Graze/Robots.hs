module Graze.Robots (disallowedBy, rules, Rules) where

import qualified Data.Text as T

import Graze.HttpUrl       (HttpUrl (..))
import Graze.Robots.Parser (parse)
import Graze.Robots.Trie   (completes, fromList, Trie)


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

rules
    :: T.Text  -- ^ User agent.
    -> T.Text  -- ^ Content of the robots.txt file.
    -> Rules
rules ua = fromList . fmap chunk . parse ua

disallowedBy :: HttpUrl -> Rules -> Bool
disallowedBy = completes . chunk . huPath
