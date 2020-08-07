{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Http
    ( ContentType (..)
    , get
    , robots
    ) where

import           Control.Exception     (try)
import qualified Data.ByteString.Char8 as C8 (ByteString, takeWhile, unpack)
import qualified Data.ByteString.Lazy  as LB (toStrict)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromMaybe)

import qualified Data.CaseInsensitive      as CI (mk)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (getGlobalManager)

import Graze.HttpUrl (HttpUrl (..), serialize)
import Graze.Robots  (Robots, parse)


data ContentType = TextHtml | TextPlain | Other

fromByteString :: C8.ByteString -> Maybe ContentType
fromByteString bs = case CI.mk (C8.takeWhile (/= ';') bs) of
    "text/html"  -> Just TextHtml
    "text/plain" -> Just TextPlain
    _            -> Nothing

type Result = (ContentType, C8.ByteString)

get :: HttpUrl -> IO Result
get url = do
    request  <- parseUrlThrow url'
    manager  <- getGlobalManager
    response <- httpLbs request manager
    let contentType = response & responseHeaders & lookup "Content-Type"
            >>= fromByteString & fromMaybe Other
    return (contentType, LB.toStrict . responseBody $ response)
  where
    url' = C8.unpack . serialize $ url

robots :: HttpUrl -> IO Robots
robots url = (try (get url') :: IO (Either HttpException Result)) <&> \case
    Left _               -> const True
    Right (TextPlain, s) -> parse "*" s
    Right _              -> const True
  where
    url' = url {huPath = "/robots.txt"}
