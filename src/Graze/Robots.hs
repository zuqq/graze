{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a lenient parser for the original robots.txt
-- <https://www.robotstxt.org/norobots-rfc.txt standard>.
module Graze.Robots
    ( Robots
    , UserAgent
    , parseRobots
    )
    where

import Control.Applicative ((<|>))
import Data.Either (isLeft, isRight, lefts, rights)
import Data.Foldable (foldl')
import Data.Set (Set)
import Data.List (find)
import Data.Text (Text)

import qualified Data.Set as Set
import qualified Data.Text as Text

import Graze.Robots.Parser
import Graze.Robots.Trie
import Graze.Robots.Types

groupLines :: [Either UserAgent Rule] -> [(Set UserAgent, [Rule])]
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

combineRules :: [Rule] -> Trie Char RuleType
combineRules = foldl' step empty
  where
    step t (Rule ruleType s) =
        if not (Text.null s)
            then insert (Text.unpack s) ruleType t
            else t

parseRules :: UserAgent -> Text -> Trie Char RuleType
parseRules userAgent =
      maybe empty combineRules
    . findGroup userAgent
    . groupLines
    . rights
    . fmap parseLine
    . Text.lines

-- | @parseRobots userAgent s@ is the predicate corresponding to the robots.txt
-- file @s@, with respect to the user agent @userAgent@.
parseRobots :: UserAgent -> Text -> Robots
parseRobots userAgent s = let t = parseRules userAgent s in
    \path ->
        case findMostSpecific path t of
            Just Disallow -> False
            _             -> True
