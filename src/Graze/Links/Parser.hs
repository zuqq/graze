{-# LANGUAGE OverloadedStrings #-}

module Graze.Links.Parser
    ( parseLink
    ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        ((<=<))
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import qualified Data.Text            as T (Text)

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

key :: A.Parser T.Text
key = A.takeWhile1 isKchar

value :: A.Parser T.Text
value = A.char '"' *> A.takeWhile1 isVchar <* A.char '"'
    <|> A.char '\'' *> A.takeWhile1 isVchar <* A.char '\''
    <|> A.takeWhile1 isVchar

attribute :: A.Parser (T.Text, T.Text)
attribute = (,)
    <$> key
    <*> A.option "" (A.skipSpace *> A.char '=' <* A.skipSpace *> value)

a :: A.Parser [(T.Text, T.Text)]
a = A.char 'a'
    *> A.takeWhile1 isSpace
    *> attribute `A.sepBy` A.skipSpace
    <* A.skipSpace

lookupHref :: [(T.Text, T.Text)] -> Either String T.Text
lookupHref = maybe (Left "No href attribute.") Right . lookup "href"

{-
    NB. This parser operates on strict 'T.Text' because that's what we get from
    properly decoding UTF-8.
-}
parseLink :: Url -> T.Text -> Either String Url
parseLink base = parseRelUrl base <=< lookupHref <=< A.parseOnly a
