{-# LANGUAGE OverloadedStrings #-}

module Graze.Links.Parser (parseLink) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text

import Graze.URI

isKeyChar :: Char -> Bool
isKeyChar c = not (isSpace c)
    && c /= '"'
    && c /= '\''
    && c /= '>'
    && c /= '/'
    && c /= '='

isValueChar :: Char -> Bool
isValueChar c = not (isSpace c)
    && c /= '"'
    && c /= '\''
    && c /= '>'

surroundedBy :: Applicative f => f a -> f b -> f a
surroundedBy p q = q *> p <* q

value :: Attoparsec.Parser Text
value = value_ `surroundedBy` Attoparsec.char '"'
    <|> value_ `surroundedBy` Attoparsec.char '\''
    <|> value_
  where
    value_ = Attoparsec.takeWhile1 isValueChar

attribute :: Attoparsec.Parser (Text, Text)
attribute =
        (,)
    <$> Attoparsec.takeWhile1 isKeyChar
    <*> Attoparsec.option ""
            (Attoparsec.char '=' `surroundedBy` Attoparsec.skipSpace *> value)

a :: Attoparsec.Parser [(Text, Text)]
a = Attoparsec.char 'a'
    *> Attoparsec.takeWhile1 isSpace
    *> attribute `Attoparsec.sepBy` Attoparsec.skipSpace

parseLink :: URI -> Text -> Maybe URI
parseLink base =
        (parseURIRelativeTo base . Text.unpack)
    <=< lookup "href"
    <=< (either (const Nothing) Just . Attoparsec.parseOnly a)
