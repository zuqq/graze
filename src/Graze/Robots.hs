{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots
    ( Rules
    , allowed
    , rules
    ) where

import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.Either          (isLeft, isRight, lefts, rights)
import qualified Data.Text            as T

import Graze.HttpUrl (HttpUrl (..))
import Graze.Trie    (Trie, completes, fromList)


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

robotsLine :: A.Parser RobotsLine
robotsLine = A.eitherP userAgent disallow

toRecords :: [RobotsLine] -> [Record]
toRecords [] = []
toRecords xs = (lefts uas, rights ds) : toRecords xs''
  where
    (uas, xs') = span isLeft xs
    (ds, xs'') = span isRight xs'

disallowsFor :: UserAgent -> [Record] -> [Disallow]
disallowsFor ua rs = [ d | (uas, ds) <- rs, ua `elem` uas, d <- ds ]

parse
    :: UserAgent
    -> T.Text      -- ^ Content of the robots.txt file.
    -> [Disallow]
parse ua = disallowsFor ua
    . toRecords
    . rights
    . fmap (A.parseOnly robotsLine)
    . T.lines

type Chunk = T.Text

type Rules = Trie Chunk

-- | Split at @'/'@, dropping any empty substrings.
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedStrings
-- >>> chunk "a"
-- ["a"]
-- >>> chunk "/a"
-- ["a"]
-- >>> chunk "a/"
-- ["a"]
-- >>> chunk "/a/b"
-- ["a","b"]
chunk :: T.Text -> [Chunk]
chunk = filter (not . T.null) . T.split (== '/')

rules
    :: UserAgent
    -> T.Text     -- ^ Content of the robots.txt file.
    -> Rules
rules ua = fromList . fmap chunk . parse ua

allowed :: HttpUrl -> Rules -> HttpUrl -> Bool
allowed base robots url =
    huDomain url == huDomain base
    && not (url `disallowedBy` robots)
  where
    disallowedBy = completes . chunk . huPath
