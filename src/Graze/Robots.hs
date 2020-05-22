{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots
    ( Robots
    , allowedBy
    , parse
    ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Either                (isLeft, isRight, lefts, rights)
import qualified Data.ByteString.Char8      as C8 (ByteString, lines, split)
import           Data.Word                  (Word8)

import Graze.Trie (Trie, completes, fromList)


type UserAgent = C8.ByteString
type Disallow  = C8.ByteString
type Line      = Either UserAgent Disallow
type Record    = ([UserAgent], [Disallow])
type Robots    = Trie C8.ByteString

-- From base's isSpace.
isSpace :: Word8 -> Bool
isSpace w = w == 32 || w - 0x9 <= 4 || w == 0xa0

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> A.takeWhile isSpace
    *> A.takeWhile (not . isSpace)

disallow :: A.Parser Disallow
disallow = A.string "Disallow:"
    *> A.takeWhile isSpace
    *> A.takeWhile (not . isSpace)

line :: A.Parser Line
line = A.eitherP userAgent disallow

group :: [Line] -> [Record]
group [] = []
group xs = (lefts uas, rights ds) : group xs''
  where
    (uas, xs') = span isLeft xs
    (ds, xs'') = span isRight xs'

targeting :: UserAgent -> [Record] -> [Disallow]
targeting ua rs = [ d | (uas, ds) <- rs, ua `elem` uas, d <- ds ]


chunk :: C8.ByteString -> [C8.ByteString]
chunk = C8.split '/'

allowedBy :: C8.ByteString -> Robots -> Bool
allowedBy = fmap not . completes . chunk

parse :: UserAgent -> C8.ByteString -> Robots
parse ua = fromList
    . fmap chunk
    . targeting ua
    . group
    . rights
    . fmap (A.parseOnly line)
    . C8.lines
