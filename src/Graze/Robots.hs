{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots
    ( Robots
    , allowedBy
    , empty
    , parse
    ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Either                (isLeft, isRight, lefts, rights)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import           Data.Word                  (Word8)

import Graze.Trie (Trie, completes, empty, fromList)

-- $setup
-- >>> :set -XOverloadedStrings


type UserAgent = B.ByteString
type Disallow  = B.ByteString
type Line      = Either UserAgent Disallow
type Record    = ([UserAgent], [Disallow])
type Robots    = Trie Word8

-- Parser ----------------------------------------------------------------------

isSpace :: Word8 -> Bool
isSpace w = w == 32  -- ' '
    || w - 9 <= 4    -- '\t', '\n', '\v', '\f', '\r'
    || w == 160      -- nbsp

isEnd :: Word8 -> Bool
isEnd w = w == 35  -- '#'
    || isSpace w

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> A.takeWhile isSpace
    *> A.takeWhile1 (not . isEnd)

disallow :: A.Parser Disallow
disallow = A.string "Disallow:"
    *> A.takeWhile isSpace
    *> A.takeWhile1 (not . isEnd)

line :: A.Parser Line
line = A.eitherP userAgent disallow

group :: [Line] -> [Record]
group [] = []
group xs = (lefts uas, rights ds) : group xs''
  where
    (uas, xs') = span isLeft xs
    (ds, xs'') = span isRight xs'

targeting :: UserAgent -> [Record] -> [Disallow]
targeting ua rs = case go ua of
    [] -> go "*"
    ds -> ds
  where
    go ua' = [ d | (uas, ds) <- rs, ua' `elem` uas, d <- ds ]

-- Interface -------------------------------------------------------------------

allowedBy :: B.ByteString -> Robots -> Bool
allowedBy = fmap not . completes . B.unpack

parse :: UserAgent -> B.ByteString -> Robots
parse ua = fromList
    . fmap B.unpack
    . targeting ua
    . group
    . rights
    . fmap (A.parseOnly line)
    . C8.lines
