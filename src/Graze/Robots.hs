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

import           Control.Applicative       ((<|>))
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.ByteString.Lazy      as BL (ByteString)
import           Data.Char                 (isAscii)
import           Data.Either               (isLeft, isRight, lefts, rights)
import           Data.Foldable             (foldl')
import           Data.Function             ((&))
import qualified Data.HashSet              as HS (HashSet, fromList, member)
import           Data.List                 (find)
import qualified Data.Text                 as T (Text, unpack)
import qualified Data.Text.Lazy            as TL (lines)
import qualified Data.Text.Lazy.Encoding   as TL (decodeUtf8With)
import           Data.Text.Encoding.Error  (lenientDecode)

import Graze.Trie (Trie, completes, empty, insert)


-- Line ------------------------------------------------------------------------

type UserAgent = T.Text

data Rule
    = Disallow  !T.Text
    | Allow     !T.Text
    | Extension !T.Text

type Line = Either UserAgent Rule

-- Predicates ------------------------------------------------------------------

isSpace :: Char -> Bool
isSpace w = w == ' ' || w == '\t'

isSafe :: Char -> Bool
isSafe w = case w of
    '$' -> True
    '-' -> True
    '_' -> True
    '.' -> True
    '+' -> True
    _   -> False

isExtra :: Char -> Bool
isExtra w = case w of
    '!'  -> True
    '*'  -> True
    '\'' -> True
    '('  -> True
    ')'  -> True
    ','  -> True
    _    -> False

isUchar :: Char -> Bool
isUchar w = w == '%'  -- Treat escaped characters by just allowing '%'.
    || 'a' <= w && w <= 'z'
    || 'A' <= w && w <= 'Z'
    || '0' <= w && w <= '9'
    || isSafe w
    || isExtra w

isPchar :: Char -> Bool
isPchar w = w == '/'  -- Just allow '/' instead of parsing segments.
    || isUchar w
    || w == ':'
    || w == '@'
    || w == '&'
    || w == '='

isCtl :: Char -> Bool
isCtl w = w < '\32' || w == '\127'

isTspecial :: Char -> Bool
isTspecial w = case w of
    '('  -> True
    ')'  -> True
    '<'  -> True
    '>'  -> True
    '@'  -> True
    ','  -> True
    ';'  -> True
    ':'  -> True
    '\\' -> True
    '"'  -> True
    '/'  -> True
    '['  -> True
    ']'  -> True
    '?'  -> True
    '='  -> True
    '{'  -> True
    '}'  -> True
    ' '  -> True
    '\t' -> True
    _    -> False

isTchar :: Char -> Bool
isTchar w = isAscii w && not (isCtl w || isTspecial w)

-- Parser ----------------------------------------------------------------------

skipSpace :: A.Parser ()
skipSpace = A.skipWhile isSpace

path :: A.Parser T.Text
path = A.takeWhile isPchar

comment :: A.Parser T.Text
comment = A.char '#' *> A.takeText

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> skipSpace
    *> A.takeWhile1 isTchar
    <* (A.endOfInput <|> skipSpace <* comment)

disallow :: A.Parser Rule
disallow = fmap Disallow $
    A.string "Disallow:"
    *> skipSpace
    *> path
    <* (A.endOfInput <|> skipSpace <* comment)

allow :: A.Parser Rule
allow = fmap Allow $
    A.string "Allow:"
    *> skipSpace
    *> path
    <* (A.endOfInput <|> skipSpace <* comment)

extension :: A.Parser Rule
extension = fmap Extension $
    A.takeWhile isTchar
    *> A.char ':'
    *> skipSpace
    *> A.takeWhile (/= '#')
    <* (A.endOfInput <|> skipSpace <* comment)

line :: A.Parser Line
line = A.eitherP userAgent (disallow <|> allow <|> extension)

-- Record ----------------------------------------------------------------------

type Record = (HS.HashSet UserAgent, (Trie Char, Trie Char))

group :: [Line] -> [Record]
group [] = []
group xs = (HS.fromList . lefts $ uas, (ds, as)) : group xs''
  where
    (uas, xs') = span isLeft xs
    (rs, xs'') = span isRight xs'
    f (u, v) r = case r of
        Disallow "" -> (u, v)
        Disallow d  -> (insert (T.unpack d) u, v)
        Allow a     -> (u, insert (T.unpack a) v)
        Extension _ -> (u, v)
    (ds, as)   = foldl' f (empty, empty) . rights $ rs

-- Robots ----------------------------------------------------------------------

-- | If we fix our user agent, a robots.txt file amounts to a predicate that is
-- @True@ for paths that we are allowed to crawl and @False@ for the others.
type Robots = T.Text -> Bool

-- | @parseRobots ua s@ is the predicate corresponding to the robots.txt file
-- @s@, with respect to the user agent @ua@.
parseRobots :: UserAgent -> BL.ByteString -> Robots
parseRobots ua s (T.unpack -> x) = not (x `completes` ds) || x `completes` as
  where
    records  = s
        & TL.decodeUtf8With lenientDecode
        & TL.lines
        & fmap (A.eitherResult . A.parse line)
        & rights
        & group
    for name = find (HS.member name . fst) records
    (ds, as) = maybe (empty, empty) snd $ for ua <|> for "*"
