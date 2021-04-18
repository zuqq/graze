{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a best-effort parser for the original robots.txt
-- <https://www.robotstxt.org/norobots-rfc.txt standard>.
module Graze.Robots
    ( Robots
    , UserAgent
    , parseRobots
    )
    where

import Control.Applicative ((<|>))
import Data.Either (isLeft, isRight, lefts, rights)
import Data.Set (Set)
import Data.List (find)
import Data.Text (Text)

import qualified Data.Set as Set
import qualified Data.Text as Text

import Graze.Robots.Parser
import Graze.Robots.Trie

groupLines :: [Line] -> [(Set UserAgent, [Rule])]
groupLines []     = []
groupLines lines_ =
    (Set.fromList (lefts userAgents), rights rules) : groupLines lines''
  where
    (userAgents, lines') = span isLeft lines_
    (rules, lines'')     = span isRight lines'

lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p = fmap snd . find (p . fst)

findGroup :: UserAgent -> [(Set UserAgent, [Rule])] -> Maybe [Rule]
findGroup userAgent groups =
        lookupBy (userAgent `Set.member`) groups
    <|> lookupBy ("*" `Set.member`) groups

combineRules :: [Rule] -> (Trie Char, Trie Char)
combineRules rules = (disallows, allows)
  where
    disallows =
        fromList [Text.unpack d | Disallow d <- rules, not (Text.null d)]
    allows    = fromList [Text.unpack a | Allow a <- rules]

parseRules :: UserAgent -> Text -> (Trie Char, Trie Char)
parseRules userAgent
    = maybe (empty, empty) combineRules
    . findGroup userAgent
    . groupLines
    . rights
    . fmap parseLine
    . Text.lines

-- | If we fix our user agent, then a robots.txt file amounts to a predicate
-- that is @True@ for paths that we are allowed to crawl and @False@ for the
-- others.
type Robots = String -> Bool

-- | @parseRobots userAgent s@ is the predicate corresponding to the robots.txt
-- file @s@, with respect to the user agent @userAgent@.
parseRobots :: UserAgent -> Text -> Robots
parseRobots userAgent s = let (disallows, allows) = parseRules userAgent s in
    \path -> not (path `completes` disallows) || path `completes` allows
