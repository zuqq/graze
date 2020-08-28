{-# LANGUAGE OverloadedStrings #-}

module Graze.Links
    ( parseLinks
    ) where

import           Control.Applicative       ((<|>))
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.ByteString.Lazy      as BL (ByteString)
import           Data.Char                 (isSpace)
import           Data.Either               (fromRight, rights)
import           Data.Functor              (($>))
import           Data.Maybe                (mapMaybe)
import qualified Data.Text                 as T (Text)
import qualified Data.Text.Lazy.Encoding   as TL (decodeUtf8With)
import           Data.Text.Encoding.Error  (lenientDecode)

import Graze.HttpUrl (HttpUrl (..), parseRelUrl)


-- Predicates ------------------------------------------------------------------

isKchar :: Char -> Bool
isKchar c = not (isSpace c)
    && c /= '"'
    && c /= '\''
    && c /= '>'
    && c /= '/'
    && c /= '='

isVchar :: Char -> Bool
isVchar c = not (isSpace c)
    && c /= '"'
    && c /= '\''
    && c /= '>'

-- Parser ----------------------------------------------------------------------

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
a = A.string "<a"
    *> A.takeWhile1 isSpace
    *> attribute `A.sepBy` A.skipSpace
    <* A.skipSpace
    <* A.char '>'

hrefs :: A.Parser [T.Text]
hrefs = mapMaybe (lookup "href") <$> go
  where
    go = A.takeWhile (/= '<') *>
        (A.endOfInput $> [] <|> (:) <$> a <*> go <|> A.char '<' *> go)

-- Interface -------------------------------------------------------------------

-- | The expression @links base html@ is a list of the URLs of all links in the
-- HTML document @html@, with @base@ serving as the base URL for relative links.
parseLinks :: HttpUrl -> BL.ByteString -> [HttpUrl]
parseLinks base = rights
    . fmap (parseRelUrl base)
    . fromRight []
    . A.eitherResult
    . A.parse hrefs
    . TL.decodeUtf8With lenientDecode
