{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Graze.Robots
    ( Robots
    , parse
    ) where

import           Control.Applicative      ((<|>))
import qualified Data.Attoparsec.Text     as A
import qualified Data.ByteString          as B (ByteString)
import           Data.Char                (isAscii)
import           Data.Either              (isLeft, isRight, lefts, rights)
import           Data.Function            ((&))
import qualified Data.HashSet             as H (HashSet, fromList, member)
import           Data.List                (find)
import qualified Data.Text                as T (Text, lines, unpack)
import qualified Data.Text.Encoding       as T (decodeUtf8With)
import qualified Data.Text.Encoding.Error as T (lenientDecode)

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

type Record = (H.HashSet UserAgent, (Trie Char, Trie Char))

group :: [Line] -> [Record]
group [] = []
group xs = (H.fromList . lefts $ uas, (ds, as)) : group xs''
  where
    (uas, xs') = span isLeft xs
    (rs, xs'') = span isRight xs'
    f r (u, v) = case r of
        Disallow "" -> (u, v)
        Disallow d  -> (insert (T.unpack d) u, v)
        Allow a     -> (u, insert (T.unpack a) v)
        Extension _ -> (u, v)
    (ds, as)   = foldr f (empty, empty) . rights $ rs

-- Robots ----------------------------------------------------------------------

type Robots = T.Text -> Bool

parse :: UserAgent -> B.ByteString -> Robots
parse ua s (T.unpack -> x) = not (x `completes` ds) || x `completes` as
  where
    records  = s
        & T.decodeUtf8With T.lenientDecode
        & T.lines
        & fmap (A.parseOnly line)
        & rights
        & group
    for name = find (H.member name . fst) records
    (ds, as) = maybe (empty, empty) snd $ for ua <|> for "*"
