{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Http
    ( ContentType (..)
    , get
    , robots
    ) where

import           Control.Exception     (try)
import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as BC (takeWhile)
import qualified Data.ByteString.Lazy  as BL (ByteString)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T (unpack)

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
fromByteString bs = case CI.mk (BC.takeWhile (/= ';') bs) of
    "text/html"  -> Just Html
    "text/plain" -> Just Plain
    _            -> Nothing

type Result = (ContentType, BL.ByteString)

get :: HttpUrl -> IO Result
get url = do
    request  <- parseUrlThrow . T.unpack . serialize $ url
    manager  <- getGlobalManager
    response <- httpLbs request manager
    let contentType = response & responseHeaders & lookup "Content-Type"
            >>= fromByteString & fromMaybe Other
    return (contentType, responseBody response)

robots :: HttpUrl -> IO Robots
robots url = (try (get url') :: IO (Either HttpException Result)) <&> \case
    Right (Plain, s) -> parse "graze" s
    _                -> const True
  where
    url' = url {huPath = "/robots.txt"}
