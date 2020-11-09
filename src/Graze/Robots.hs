{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

--------------------------------------------------------------------------------
-- | Module: Graze.Robots
--
-- This module contains a best-effort parser for the original robots.txt
-- <https://www.robotstxt.org/norobots-rfc.txt standard>.
--
-- = Implementation
--
-- The parser has two stages; it first parsese the individual lines and folds
-- them into groups. The line parser is a straightforward translation of the
-- grammar involving a handful of predicates for admissible characters,
-- primitives from "Data.Attoparse.Text.Lazy", and the 'Applicative' class.
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
--------------------------------------------------------------------------------

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

import Graze.Robots.Parser (Line, Rule(..), UserAgent, parseLine)
import Graze.Trie          (Trie, completes, empty, insert)


-- Record ----------------------------------------------------------------------

type RuleSet = (Trie Char, Trie Char)

emptyRuleSet :: RuleSet
emptyRuleSet = (empty, empty)

fromList :: [Rule] -> RuleSet
fromList = foldl' step emptyRuleSet
  where
    step (!disallows, !allows) = \case
        Disallow "" -> (disallows, allows)
        Disallow d  -> (insert (T.unpack d) disallows, allows)
        Allow a     -> (disallows, insert (T.unpack a) allows)
        Extension _ -> (disallows, allows)

type Record = (HS.HashSet UserAgent, RuleSet)

affects :: UserAgent -> Record -> Bool
affects userAgent = HS.member userAgent . fst

ruleSet :: Record -> RuleSet
ruleSet = snd

-- | Returns, in descending priority, one of:
--
-- * the 'RuleSet' of the first 'Record' that mentions the given 'UserAgent';
-- * the 'RuleSet' of the first 'Record' that mentions @\"*\"@;
-- * the empty 'RuleSet'.
extract :: UserAgent -> [Record] -> RuleSet
extract userAgent records = maybe emptyRuleSet ruleSet $ go userAgent <|> go "*"
  where
    go x = find (affects x) records

group :: [Line] -> [Record]
group [] = []
group ls = (HS.fromList userAgents, fromList rules) : group ls''
  where
    (lefts -> userAgents, ls') = span isLeft ls
    (rights -> rules, ls'')    = span isRight ls'

-- Robots ----------------------------------------------------------------------

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
        & group
        & extract userAgent
