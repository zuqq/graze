{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots.Parser
    ( Line
    , Rule (..)
    , UserAgent
    , parseLine
    )
    where

import Control.Applicative ((<|>))
import Data.CaseInsensitive (CI)
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.CaseInsensitive as CI

type UserAgent = CI Text

data Rule = Disallow !Text | Allow !Text

type Line = Either UserAgent Rule

nonSpecial :: Char -> Bool
nonSpecial c =
    case c of
        ' '  -> False
        '\t' -> False
        '#'  -> False
        _    -> True

userAgent :: Attoparsec.Parser UserAgent
userAgent = CI.mk <$> ("User-agent:" *> Attoparsec.skipSpace *> userAgent_)
  where
    userAgent_ = Attoparsec.takeWhile1 nonSpecial

path :: Attoparsec.Parser Text
path = Attoparsec.takeWhile nonSpecial

disallow :: Attoparsec.Parser Rule
disallow = Disallow <$> ("Disallow:" *> Attoparsec.skipSpace *> path)

allow :: Attoparsec.Parser Rule
allow = Allow <$> ("Allow:" *> Attoparsec.skipSpace *> path)

line :: Attoparsec.Parser Line
line = Attoparsec.eitherP userAgent (disallow <|> allow)

parseLine :: Text -> Either String Line
parseLine = Attoparsec.parseOnly line
