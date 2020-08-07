{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Http
    ( ContentType (..)
    , get
    , robots
    ) where

import           Control.Exception     (try)
import qualified Data.ByteString.Char8 as C8 (takeWhile, unpack)
import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Lazy  as L (ByteString, toStrict)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromMaybe)

import qualified Data.CaseInsensitive      as CI (mk)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (getGlobalManager)

import Graze.HttpUrl (HttpUrl (..), serialize)
import Graze.Robots  (Robots, parse)


data ContentType
    = Html   -- ^ text/html
    | Plain  -- ^ text/plain
    | Other

fromByteString :: B.ByteString -> Maybe ContentType
fromByteString bs = case CI.mk (C8.takeWhile (/= ';') bs) of
    "text/html"  -> Just Html
    "text/plain" -> Just Plain
    _            -> Nothing

type Result = (ContentType, L.ByteString)

get :: HttpUrl -> IO Result
get url = do
    request  <- parseUrlThrow url'
    manager  <- getGlobalManager
    response <- httpLbs request manager
    let contentType = response & responseHeaders & lookup "Content-Type"
            >>= fromByteString & fromMaybe Other
    return (contentType, responseBody response)
  where
    url' = C8.unpack . serialize $ url

robots :: HttpUrl -> IO Robots
robots url = (try (get url') :: IO (Either HttpException Result)) <&> \case
    Left _           -> const True
    Right (Plain, s) -> parse "*" . L.toStrict $ s
    Right _          -> const True
  where
    url' = url {huPath = "/robots.txt"}
