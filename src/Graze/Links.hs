{-# LANGUAGE OverloadedStrings #-}

module Graze.Links
    ( links
    ) where

import           Control.Applicative      ((<|>))
import qualified Data.Attoparsec.Text     as A
import qualified Data.ByteString          as B (ByteString)
import           Data.Char                (isSpace)
import           Data.Either              (fromRight, rights)
import           Data.Functor             (($>))
import qualified Data.HashSet             as H (fromList, toList)
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T (Text)
import qualified Data.Text.Encoding       as T (decodeUtf8With)
import qualified Data.Text.Encoding.Error as T (lenientDecode)

import Graze.HttpUrl (HttpUrl (..), parseRel)


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
links :: HttpUrl -> B.ByteString -> [HttpUrl]
links base = H.toList
    . H.fromList
    . rights
    . fmap (parseRel base)
    . fromRight []
    . A.parseOnly hrefs
    . T.decodeUtf8With T.lenientDecode
