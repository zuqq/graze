{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a lenient parser for the original robots.txt
-- <https://www.robotstxt.org/norobots-rfc.txt standard>.
module Graze.Robots (Robots, parseRobots) where

import Data.Either (isLeft, isRight, rights)
import Data.Foldable (foldl')
import Data.Text (Text)

import qualified Data.Text as Text

import Graze.Robots.Parser
import Graze.Robots.Trie

-- | If we fix our user agent, then a robots.txt file amounts to a predicate
-- that is @True@ for paths that we are allowed to crawl and @False@ for the
-- others.
type Robots = String -> Bool

findRules :: Text -> [Either Text Rule] -> [Rule]
findRules userAgent =
      rights
    . takeWhile isRight
    . dropWhile isLeft
    . dropWhile (/= Left userAgent)

combineRules :: [Rule] -> Trie Char RuleType
combineRules = foldl' step empty
  where
    step t (Rule ruleType s) =
        if not (Text.null s)
            then insert (Text.unpack s) ruleType t
            else t

parseRules :: Text -> Text -> Trie Char RuleType
parseRules userAgent =
      combineRules
    . findRules userAgent
    . rights
    . fmap parseLine
    . Text.lines

-- | @parseRobots s@ is the predicate corresponding to the robots.txt file @s@,
-- with respect to the user agent @\"*\"@.
parseRobots :: Text -> Robots
parseRobots s =
    \path ->
        case findMostSpecific path t of
            Just Disallow -> False
            _             -> True
  where
    t = parseRules "*" s
