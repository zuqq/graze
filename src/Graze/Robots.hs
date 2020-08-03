{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Graze.Robots
    ( Robots
    , parse
    ) where

import           Control.Applicative        ((<|>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.Either                (isLeft, isRight, lefts, rights)
import qualified Data.HashSet               as H
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Data.Word                  (Word8)

import Graze.Internal (isPchar)
import Graze.Trie     (Trie, completes, empty, insert)


-- Line ------------------------------------------------------------------------

type UserAgent = B.ByteString

data Rule
    = Disallow  !B.ByteString
    | Allow     !B.ByteString
    | Extension !B.ByteString

type Line = Either UserAgent Rule

-- Predicates ------------------------------------------------------------------

isSpace :: Word8 -> Bool
isSpace w = w == 9 || w == 32

isCtl :: Word8 -> Bool
isCtl w = w < 32 || w == 127

isTspecial :: Word8 -> Bool
isTspecial w = case w of
    40  -> True  -- '('
    41  -> True  -- ')'
    60  -> True  -- '<'
    62  -> True  -- '>'
    64  -> True  -- '@'
    44  -> True  -- ','
    59  -> True  -- ';'
    58  -> True  -- ':'
    92  -> True  -- '\\'
    34  -> True  -- '"'
    47  -> True  -- '/'
    91  -> True  -- '['
    93  -> True  -- ']'
    63  -> True  -- '?'
    61  -> True  -- '='
    123 -> True  -- '{'
    125 -> True  -- '}'
    32  -> True  -- ' '
    9   -> True  -- '\t'
    _   -> False

isTchar :: Word8 -> Bool
isTchar w = not (isCtl w || isTspecial w)

-- Parser ----------------------------------------------------------------------

#define POUND 35
#define COLON 58

skipSpace :: A.Parser ()
skipSpace = A.skipWhile isSpace

path :: A.Parser B.ByteString
path = A.takeWhile isPchar

comment :: A.Parser B.ByteString
comment = A.word8 POUND *> A.takeByteString

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
    *> A.word8 COLON
    *> skipSpace
    *> A.takeWhile (/= POUND)
    <* (A.endOfInput <|> skipSpace <* comment)

line :: A.Parser Line
line = A.eitherP userAgent (disallow <|> allow <|> extension)

-- Record ----------------------------------------------------------------------

type Record = (H.HashSet UserAgent, (Trie Word8, Trie Word8))

group :: [Line] -> [Record]
group [] = []
group xs = (H.fromList . lefts $ uas, (ds, as)) : group xs''
  where
    (uas, xs') = span isLeft xs
    (rs, xs'') = span isRight xs'
    f r (u, v) = case r of
        Disallow "" -> (u, v)
        Disallow d  -> (insert (B.unpack d) u, v)
        Allow a     -> (u, insert (B.unpack a) v)
        Extension _ -> (u, v)
    (ds, as)   = foldr f (empty, empty) . rights $ rs

-- Robots ----------------------------------------------------------------------

type Robots = B.ByteString -> Bool

parse :: UserAgent -> B.ByteString -> Robots
parse ua s (B.unpack -> x) = not (x `completes` ds) || x `completes` as
  where
    records  = group . rights . fmap (A.parseOnly line) . C.lines $ s
    for name = find (H.member name . fst) records
    (ds, as) = fromMaybe (empty, empty) . fmap snd $ for ua <|> for "*"
