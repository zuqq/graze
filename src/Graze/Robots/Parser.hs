{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots.Parser (parse) where

import           Data.Either    (isRight, rights)
import           Data.Maybe     (mapMaybe)
import qualified Data.Text.Lazy as TL


type UserAgent = TL.Text

type Disallow = TL.Text

type RobotsLine = Either UserAgent Disallow

data Record = Record
    { userAgent :: !UserAgent
    , disallows :: ![Disallow]
    }

parseLine :: TL.Text -> Maybe RobotsLine
parseLine l = case takeWhile noComment (TL.words l) of
    ["User-agent:", ua] -> Just (Left ua)
    ["Disallow:", d]    -> Just (Right d)
    _                   -> Nothing
  where
    noComment = (/= '#') . TL.head

toRecords :: [RobotsLine] -> [Record]
toRecords rls = case rls of
    []               -> []
    (Right _ : _)    -> toRecords $ dropWhile isRight rls
    (Left ua : rest) -> let (ds, rest') = span isRight rest in
                        Record ua (rights ds) : toRecords rest'

disallowsFor :: UserAgent -> [Record] -> [Disallow]
disallowsFor ua rs = [ d | r <- rs, userAgent r == ua, d <- disallows r ]

parse :: TL.Text -> [Disallow]
parse = disallowsFor "*" . toRecords . mapMaybe parseLine . TL.lines
