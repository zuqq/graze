{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots.Parser (parse) where

import           Data.Either (isRight, rights)
import           Data.Maybe  (mapMaybe)
import qualified Data.Text   as T


type UserAgent = T.Text

type Disallow = T.Text

type RobotsLine = Either UserAgent Disallow

data Record = Record
    { userAgent :: !UserAgent
    , disallows :: ![Disallow]
    }

parseLine :: T.Text -> Maybe RobotsLine
parseLine l = case takeWhile noComment (T.words l) of
    ["User-agent:", ua] -> Just (Left ua)
    ["Disallow:", d]    -> Just (Right d)
    _                   -> Nothing
  where
    noComment = (/= '#') . T.head

toRecords :: [RobotsLine] -> [Record]
toRecords []               = []
toRecords (Right _ : rest) = toRecords rest
toRecords (Left ua : rest) = Record ua (rights ds) : toRecords rest'
  where
    (ds, rest') = span isRight rest

disallowsFor :: UserAgent -> [Record] -> [Disallow]
disallowsFor ua rs = [ d | r <- rs, userAgent r == ua, d <- disallows r ]

parse :: T.Text -> [Disallow]
parse = disallowsFor "*" . toRecords . mapMaybe parseLine . T.lines
