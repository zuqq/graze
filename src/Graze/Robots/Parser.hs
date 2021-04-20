{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots.Parser (parseLine) where

import Control.Applicative ((<|>))
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Attoparsec

import Graze.Robots.Types

nonSpecial :: Char -> Bool
nonSpecial c = not (Attoparsec.isHorizontalSpace c) && c /= '#'

userAgent :: Attoparsec.Parser Text
userAgent =
        Attoparsec.asciiCI "User-agent:"
    *>  Attoparsec.skipSpace
    *>  Attoparsec.takeWhile1 nonSpecial

disallow :: Attoparsec.Parser Rule
disallow =
        Attoparsec.asciiCI "Disallow:"
    *>  Attoparsec.skipSpace
    *>  (Rule Disallow <$> Attoparsec.takeWhile nonSpecial)

allow :: Attoparsec.Parser Rule
allow =
        Attoparsec.asciiCI "Allow:"
    *>  Attoparsec.skipSpace
    *>  (Rule Allow <$> Attoparsec.takeWhile nonSpecial)

parseLine :: Text -> Either String (Either Text Rule)
parseLine =
    Attoparsec.parseOnly (Attoparsec.eitherP userAgent (disallow <|> allow))
