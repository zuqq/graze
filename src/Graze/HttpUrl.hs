{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl
    ( HttpUrl (..)
    , hash
    , parse
    , parseRel
    , serialize
    ) where

import           Control.Applicative  ((<|>), liftA2, liftA3)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T (Text, cons, dropWhileEnd)
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
scheme = A.string "https:" <|> A.string "http:"

domain :: A.Parser T.Text
domain = liftA2 (<>) (A.string "//") (A.takeWhile (/= '/'))

path :: A.Parser T.Text
path = liftA2 T.cons (A.char '/') A.takeText

url :: A.Parser HttpUrl
url = liftA3 HttpUrl scheme domain (path <|> pure "/")

relUrl :: HttpUrl -> A.Parser HttpUrl
relUrl HttpUrl {..} = liftA3 HttpUrl
    (scheme <|> pure huScheme)
    (domain <|> pure huDomain)
    (path   <|> relPath)
  where
    huFolder = T.dropWhileEnd (/= '/') huPath
    relPath  = liftA2 (<>) (pure huFolder) A.takeText

-- | Parse an absolute URL.
parse :: T.Text -> Either String HttpUrl
parse = A.parseOnly url

-- | Parse a relative or absolute URL; if it is relative, interpret it relative
-- to the first argument.
parseRel :: HttpUrl -> T.Text -> Either String HttpUrl
parseRel = A.parseOnly . relUrl
