{-# LANGUAGE OverloadedStrings #-}

module Graze.Http
    ( request
    , robots
    ) where

import           Control.Exception  (try)
import           Control.Monad      ((<=<))
import           Data.Either        (fromRight)
import qualified Data.ByteString    as B (ByteString)
import qualified Data.Text          as T (unpack)
import qualified Data.Text.Encoding as T (decodeUtf8)

import Network.HTTP.Conduit (HttpException, parseUrlThrow, redirectCount)
import Network.HTTP.Simple  (getResponseBody, httpBS)

import Graze.HttpUrl (HttpUrl (..), serialize)
import Graze.Robots  (Rules, rules)


request :: HttpUrl -> IO B.ByteString
request = fmap getResponseBody
    . httpBS
    <=< fmap noFollow
    . parseUrlThrow
    . T.unpack
    . serialize
  where
    noFollow r = r {redirectCount = 0}

robots :: HttpUrl -> IO Rules
robots url = rules "*" . T.decodeUtf8 . fromRight "" <$>
    (try (request url') :: IO (Either HttpException B.ByteString))
  where
    url' = url {huPath = "/robots.txt"}
