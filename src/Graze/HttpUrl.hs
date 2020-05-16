{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl
    ( HttpUrl (..)
    , hash
    , parse
    , parseRel
    , serialize
    ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        ((>=>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T (Text, cons, dropWhileEnd, snoc)
import qualified Data.Text.Encoding   as T (encodeUtf8)

import qualified Crypto.Hash.SHA1       as SHA1   (hash)
import qualified Data.ByteString.Base16 as Base16 (encode)

-- $setup
-- >>> :set -XOverloadedStrings


data HttpUrl = HttpUrl
    { huScheme :: !T.Text
    , huDomain :: !T.Text
    , huPath   :: !T.Text
    }
    deriving Show

-- |
-- >>> x = HttpUrl "http:" "//x" "/y"
-- >>> y = HttpUrl "https:" "//x" "/y"
-- >>> x == y
-- True
instance Eq HttpUrl where
    x == y = (huDomain x, huPath x) == (huDomain y, huPath y)

-- |
-- >>> x = HttpUrl "https:" "//x" "/"
-- >>> y = HttpUrl "http:" "//x" "/y"
-- >>> x < y
-- True
instance Ord HttpUrl where
    x <= y = (huDomain x, huPath x) <= (huDomain y, huPath y)

serialize :: HttpUrl -> T.Text
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

-- | Map an 'HttpUrl' to its SHA-1 digest.
hash :: HttpUrl -> String
hash = show . Base16.encode . SHA1.hash . T.encodeUtf8 . serialize

scheme :: A.Parser T.Text
scheme = T.snoc <$> A.takeWhile (/= ':') <*> A.char ':'

domain :: A.Parser T.Text
domain = (<>) <$> A.string "//" <*> A.takeWhile (/= '/')

path :: A.Parser T.Text
path = T.cons <$> A.char '/' <*> A.takeText

url :: A.Parser HttpUrl
url = HttpUrl
    <$> scheme
    <*> domain
    <*> (path <|> pure "/")

relUrl :: HttpUrl -> A.Parser HttpUrl
relUrl HttpUrl {..} = HttpUrl
    <$> (scheme <|> pure huScheme)
    <*> (domain <|> pure huDomain)
    <*> (path   <|> relPath)
  where
    huFolder = T.dropWhileEnd (/= '/') huPath
    relPath  = (<>) <$> pure huFolder <*> A.takeText

checkScheme :: HttpUrl -> Either String HttpUrl
checkScheme x@(HttpUrl s _ _) = case s of
    "http:"  -> Right x
    "https:" -> Right x
    _        -> Left "Scheme is not HTTP(S)"

-- | Parse an absolute HTTP(S) URL.
parse :: T.Text -> Either String HttpUrl
parse = A.parseOnly url >=> checkScheme

-- | Parse an HTTP(S) URL relative to the first argument.
parseRel :: HttpUrl -> T.Text -> Either String HttpUrl
parseRel x = A.parseOnly (relUrl x) >=> checkScheme
