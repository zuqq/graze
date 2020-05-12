{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots.Parser (parse) where

import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.Either          (isLeft, isRight, lefts, rights)
import qualified Data.Text            as T (Text, lines)


type UserAgent = T.Text

type Disallow = T.Text

type RobotsLine = Either UserAgent Disallow

type Record = ([UserAgent], [Disallow])

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> A.skipSpace
    *> A.takeWhile (not . isSpace)

disallow :: A.Parser Disallow
disallow = A.string "Disallow:"
    *> A.skipSpace
    *> A.takeWhile (not . isSpace)

line :: A.Parser RobotsLine
line = A.eitherP userAgent disallow

toRecords :: [RobotsLine] -> [Record]
toRecords [] = []
toRecords xs = (lefts uas, rights ds) : toRecords xs''
  where
    (uas, xs') = span isLeft xs
    (ds, xs'') = span isRight xs'

disallowsFor :: UserAgent -> [Record] -> [Disallow]
disallowsFor ua rs = [ d | (uas, ds) <- rs, ua `elem` uas, d <- ds ]

parse
    :: T.Text      -- ^ User agent.
    -> T.Text      -- ^ Content of the robots.txt file.
    -> [T.Text]
parse ua = disallowsFor ua
    . toRecords
    . rights
    . fmap (A.parseOnly line)
    . T.lines
