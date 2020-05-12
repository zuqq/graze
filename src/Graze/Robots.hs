{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots (disallowedBy, rules, Rules) where

import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.Either          (isLeft, isRight, lefts, rights)
import qualified Data.Text            as T

import Graze.HttpUrl (HttpUrl (..))
import Graze.Trie    (completes, fromList, Trie)


type UserAgent = T.Text

type Disallow = T.Text

type RobotsLine = Either UserAgent Disallow

type Record = ([UserAgent], [Disallow])

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> A.skipSpace
    *> A.takeWhile (not . isSpace)

disallow :: A.Parser Disallow
disallow = A.string "Disallow:"
    *> A.skipSpace
    *> A.takeWhile (not . isSpace)

line :: A.Parser RobotsLine
line = A.eitherP userAgent disallow

toRecords :: [RobotsLine] -> [Record]
toRecords [] = []
toRecords xs = (lefts uas, rights ds) : toRecords xs''
  where
    (uas, xs') = span isLeft xs
    (ds, xs'') = span isRight xs'

disallowsFor :: UserAgent -> [Record] -> [Disallow]
disallowsFor ua rs = [ d | (uas, ds) <- rs, ua `elem` uas, d <- ds ]

parse
    :: T.Text      -- ^ User agent.
    -> T.Text      -- ^ Content of the robots.txt file.
    -> [T.Text]
parse ua = disallowsFor ua
    . toRecords
    . rights
    . fmap (A.parseOnly line)
    . T.lines

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
