{-# LANGUAGE OverloadedStrings #-}

module Graze.Links.Parser
    ( parseLink
    )
    where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Attoparsec

import Graze.Url

isKchar :: Char -> Bool
isKchar c = not (isSpace c)
    && c /= '"'
    && c /= '\''
    && c /= '>'
    && c /= '/'
    && c /= '='

isVchar :: Char -> Bool
isVchar c = not (isSpace c)
    && c /= '"'
    && c /= '\''
    && c /= '>'

key :: Attoparsec.Parser Text
key = Attoparsec.takeWhile1 isKchar

value :: Attoparsec.Parser Text
value = Attoparsec.char '"' *> Attoparsec.takeWhile1 isVchar <* Attoparsec.char '"'
    <|> Attoparsec.char '\'' *> Attoparsec.takeWhile1 isVchar <* Attoparsec.char '\''
    <|> Attoparsec.takeWhile1 isVchar

attribute :: Attoparsec.Parser (Text, Text)
attribute = (,)
    <$> key
    <*> Attoparsec.option "" (Attoparsec.skipSpace *> Attoparsec.char '=' <* Attoparsec.skipSpace *> value)

a :: Attoparsec.Parser [(Text, Text)]
a = Attoparsec.char 'a'
    *> Attoparsec.takeWhile1 isSpace
    *> attribute `Attoparsec.sepBy` Attoparsec.skipSpace
    <* Attoparsec.skipSpace

lookupHref :: [(Text, Text)] -> Either String Text
lookupHref = maybe (Left "No href attribute.") Right . lookup "href"

-- NB. This parser operates on strict 'Text' because that's what we get from
-- properly decoding UTF-8.
parseLink :: Url -> Text -> Either String Url
parseLink base = parseRelUrl base <=< lookupHref <=< Attoparsec.parseOnly a
