{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots
    ( Robots
    , allowedBy
    , parse
    ) where

import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.Either          (isLeft, isRight, lefts, rights)
import qualified Data.Text            as T

import Graze.Trie (Trie, completes, fromList)


type UserAgent = T.Text

type Disallow = T.Text

type Line = Either UserAgent Disallow

type Record = ([UserAgent], [Disallow])

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> A.skipSpace
    *> A.takeWhile (not . isSpace)

disallow :: A.Parser Disallow
disallow = A.string "Disallow:"
    *> A.skipSpace
    *> A.takeWhile (not . isSpace)

robotsLine :: A.Parser Line
robotsLine = A.eitherP userAgent disallow

toRecords :: [Line] -> [Record]
toRecords [] = []
toRecords xs = (lefts uas, rights ds) : toRecords xs''
  where
    (uas, xs') = span isLeft xs
    (ds, xs'') = span isRight xs'

disallowsFor :: UserAgent -> [Record] -> [Disallow]
disallowsFor ua rs = [ d | (uas, ds) <- rs, ua `elem` uas, d <- ds ]

type Robots = Trie T.Text

chunk :: T.Text -> [T.Text]
chunk = T.split (== '/')

parse :: UserAgent -> T.Text -> Robots
parse ua = fromList
    . fmap chunk
    . disallowsFor ua
    . toRecords
    . rights
    . fmap (A.parseOnly robotsLine)
    . T.lines

allowedBy :: T.Text -> Robots -> Bool
allowedBy = fmap not . completes . chunk
