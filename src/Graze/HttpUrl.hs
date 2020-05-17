{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl
    ( HttpUrl (huDomain, huPath, huScheme)
    , hash
    , parse
    , parseRelTo
    , serialize
    ) where

import           Control.Applicative  ((<|>), liftA2, liftA3)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T (encodeUtf8)

import qualified Crypto.Hash.SHA1       as SHA1   (hash)
import qualified Data.ByteString.Base16 as Base16 (encode)


data HttpUrl = HttpUrl
    { huScheme :: !T.Text
    , huDomain :: !T.Text
    , huPath   :: !T.Text
    }

instance Eq HttpUrl where
    x == y = (huDomain x, huPath x) == (huDomain y, huPath y)

instance Ord HttpUrl where
    x <= y = (huDomain x, huPath x) <= (huDomain y, huPath y)

serialize :: HttpUrl -> T.Text
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

-- | Map an 'HttpUrl' to its SHA-1 digest.
hash :: HttpUrl -> String
hash = show . Base16.encode . SHA1.hash . T.encodeUtf8 . serialize

scheme :: A.Parser T.Text
scheme = liftA2 T.snoc (A.takeWhile (/= ':')) (A.char ':') >>= \case
    "http:"  -> return "http:"
    "https:" -> return "https:"
    _        -> fail "scheme"

domain :: A.Parser T.Text
domain = liftA2 (<>) (A.string "//") (A.takeWhile (/= '/'))

splitPath :: T.Text -> (T.Text, T.Text)
splitPath = T.breakOn "?" . fst . T.breakOn "#"

normalize :: T.Text -> T.Text
normalize s =
    let (p, q)  = splitPath s
        chunks  = filter (not . T.null) . T.split (== '/') $ p
        suffix  = if "/" `T.isSuffixOf` p && chunks /= [] then "/" else ""
        chunks' = go [] chunks
    in "/" <> T.intercalate "/" chunks' <> suffix <> q
  where
    go xs (".." : ys) = go (drop 1 xs) ys
    go xs ("." : ys)  = go xs ys
    go xs (y : ys)    = go (y : xs) ys
    go xs []          = reverse xs

path :: A.Parser T.Text
path = normalize <$> liftA2 T.cons (A.char '/') A.takeText

relPath :: T.Text -> A.Parser T.Text
relPath folder = normalize <$> liftA2 (<>) (pure folder) A.takeText

url :: A.Parser HttpUrl
url = liftA3 HttpUrl scheme domain (path <|> pure "/")

relTo :: HttpUrl -> A.Parser HttpUrl
relTo HttpUrl {..} = liftA3 HttpUrl
    (scheme <|> pure huScheme)
    (domain <|> pure huDomain)
    (path   <|> relPath huFolder)
  where
    huFolder = T.dropWhileEnd (/= '/') . fst . splitPath $ huPath

-- | Parse an absolute HTTP(S) URL.
parse :: T.Text -> Either String HttpUrl
parse = A.parseOnly url

-- | Parse an HTTP(S) URL relative to the first argument.
parseRelTo :: HttpUrl -> T.Text -> Either String HttpUrl
parseRelTo x = A.parseOnly (relTo x)
