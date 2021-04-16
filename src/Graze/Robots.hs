{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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
import Data.HashSet (HashSet)
import Data.List (find)
import Data.Text (Text)

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Graze.Robots.Parser
import Graze.Robots.Trie

groupLines :: [Line] -> [(HashSet UserAgent, [Rule])]
groupLines []     = []
groupLines lines_ = (HashSet.fromList userAgents, rules) : groupLines lines''
  where
    (lefts -> userAgents, lines') = span isLeft lines_
    (rights -> rules, lines'')    = span isRight lines'

findGroup :: UserAgent -> [(HashSet UserAgent, [Rule])] -> Maybe [Rule]
findGroup userAgent groups = snd <$> (find_ userAgent <|> find_ "*")
  where
    find_ userAgent_ = find ((userAgent_ `HashSet.member`) . fst) groups

combineRules :: [Rule] -> (Trie Char, Trie Char)
combineRules rules = (fromList disallows, fromList allows)
  where
    disallows = [Text.unpack d | Disallow d <- rules, not (Text.null d)]
    allows    = [Text.unpack a | Allow a <- rules]

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
type Robots = Text -> Bool

-- | @parseRobots userAgent s@ is the predicate corresponding to the robots.txt
-- file @s@, with respect to the user agent @userAgent@.
parseRobots :: UserAgent -> Text -> Robots
parseRobots userAgent s = let (disallows, allows) = parseRules userAgent s in
    \(Text.unpack -> x) -> not (x `completes` disallows) || x `completes` allows
