{-# LANGUAGE OverloadedStrings #-}

module Graze.Http
    ( request
    , robots
    ) where

import           Control.Exception     (try)
import           Data.Either           (fromRight)
import qualified Data.ByteString.Char8 as C8 (ByteString, unpack)
import qualified Data.ByteString.Lazy  as LB (toStrict)

import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)

import Graze.HttpUrl (HttpUrl (..), serialize)
import Graze.Robots  (Robots, parse)


request :: HttpUrl -> IO C8.ByteString
request url = do
    m <- getGlobalManager
    r <- parseUrlThrow url'
    LB.toStrict . responseBody <$> httpLbs r {redirectCount = 0} m
  where
    url' = C8.unpack . serialize $ url

robots :: HttpUrl -> IO Robots
robots url = parse "*" . fromRight "" <$>
    (try (request url') :: IO (Either HttpException C8.ByteString))
  where
    url' = url {huPath = "/robots.txt"}
