{-# LANGUAGE OverloadedStrings #-}

-- | A lenient parser for robots.txt files.
module Graze.Robots (Robots, parseRobots) where

import Data.Either (isLeft, isRight, rights)
import Data.Foldable (foldl')
import Data.Text (Text)

import qualified Data.Text as Text

import Graze.Robots.Parser
import Graze.Robots.Trie

-- | If we fix our user agent, then a robots.txt file amounts to a predicate
-- that is @True@ for paths that we are allowed to crawl and @False@ for all
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
    step t (Rule ruleType s)
        | Text.null s = t
        | otherwise   = insert (Text.unpack s) ruleType t

parseRules :: Text -> Text -> Trie Char RuleType
parseRules userAgent =
      combineRules
    . findRules userAgent
    . rights
    . fmap parseLine
    . Text.lines

-- | Parse a robots.txt file with respect to the generic user agent @*@.
parseRobots :: Text -> Robots
parseRobots s =
    \path ->
        case findMostSpecific path t of
            Just Disallow -> False
            _             -> True
  where
    t = parseRules "*" s
