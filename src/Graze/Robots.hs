{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains a best-effort parser for the original robots.txt
-- <https://www.robotstxt.org/norobots-rfc.txt standard>.
--
-- = Implementation
--
-- Since robots.txt is a line-based format, we first parse each line separately
-- before combining them into meaningful directives.
--
-- = Deficiencies
--
-- The parser deviates from the standard in (at least) the following points:
--
--     * User agents are matched exactly, not in a case-insensitive way.
--     * There is no special handling of @\"/robots.txt\"@.
--     * There is no special handling of URL-encoded paths.
--     * Paths aren't required to be absolute.
--     * If a path is affected by both an \"Allow\" and a \"Disallow\", then the
--       \"Allow\" wins out; the standard intends the rule that occurs first to
--       be decisive.

module Graze.Robots
    ( Robots
    , UserAgent
    , parseRobots
    ) where

import           Control.Applicative ((<|>))
import           Data.Either         (isLeft, isRight, lefts, rights)
import           Data.Foldable       (foldl')
import           Data.Function       ((&))
import qualified Data.HashSet        as HS (HashSet, fromList, member)
import           Data.List           (find)
import qualified Data.Text           as T (Text, lines, unpack)

import Graze.Robots.Parser
import Graze.Robots.Trie


type RuleSet
    = ( Trie Char  -- Disallows
      , Trie Char  -- Allows
      )

combineRules :: [Rule] -> RuleSet
combineRules = foldl' step (empty, empty)
  where
    step (!disallows, !allows) = \case
        Disallow "" -> (disallows, allows)
        Disallow d  -> (insert (T.unpack d) disallows, allows)
        Allow a     -> (disallows, insert (T.unpack a) allows)
        Extension _ -> (disallows, allows)

type Record = (HS.HashSet UserAgent, RuleSet)

affects :: Record -> UserAgent -> Bool
affects = flip go
  where
    go x = HS.member x . fst

ruleSet :: Record -> RuleSet
ruleSet = snd

groupLines :: [Line] -> [Record]
groupLines [] = []
groupLines ls = (HS.fromList userAgents, combineRules rules) : groupLines ls''
  where
    (lefts -> userAgents, ls') = span isLeft ls
    (rights -> rules, ls'')    = span isRight ls'

-- | Returns, in descending priority, one of:
--
--     * the 'RuleSet' of the first 'Record' that affects the 'UserAgent';
--     * the 'RuleSet' of the first 'Record' that affects @\"*\"@;
--     * the empty 'RuleSet'.
findRuleSetFor :: UserAgent -> [Record] -> RuleSet
findRuleSetFor userAgent records = maybe (empty, empty) ruleSet $
    go userAgent <|> go "*"
  where
    go x = find (`affects` x) records

-- | If we fix our user agent, a robots.txt file amounts to a predicate that is
-- @True@ for paths that we are allowed to crawl and @False@ for the others.
type Robots = T.Text -> Bool

-- | @parseRobots userAgent s@ is the predicate corresponding to the robots.txt
-- file @s@, with respect to the user agent @userAgent@.
parseRobots :: UserAgent -> T.Text -> Robots
parseRobots userAgent s = \(T.unpack -> x) ->
    not (x `completes` disallows) || x `completes` allows
  where
    (!disallows, !allows) = s
        & T.lines
        & fmap parseLine
        & rights
        & groupLines
        & findRuleSetFor userAgent
