{-# LANGUAGE OverloadedStrings #-}

module Graze.Http
    ( request
    , robots
    ) where

import           Control.Exception    (try)
import           Data.Either          (fromRight)
import qualified Data.ByteString      as B  (ByteString)
import qualified Data.ByteString.Lazy as LB (toStrict)
import qualified Data.Text            as T  (unpack)
import qualified Data.Text.Encoding   as T  (decodeUtf8)

import Network.HTTP.Client
    ( HttpException
    , httpLbs
    , parseUrlThrow
    , redirectCount
    , responseBody
    )
import Network.HTTP.Client.TLS (getGlobalManager)

import Graze.HttpUrl (HttpUrl (..), serialize)
import Graze.Robots  (Robots, parse)


request :: HttpUrl -> IO B.ByteString
request url = do
    m <- getGlobalManager
    r <- parseUrlThrow url'
    LB.toStrict . responseBody <$> httpLbs r {redirectCount = 0} m
  where
    url' = T.unpack . serialize $ url

robots :: HttpUrl -> IO Robots
robots url = parse "*" . T.decodeUtf8 . fromRight "" <$>
    (try (request url') :: IO (Either HttpException B.ByteString))
  where
    url' = url {huPath = "/robots.txt"}
