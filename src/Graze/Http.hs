{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Http
    ( ContentType (..)
    , get
    , getRobots
    ) where

import           Control.Exception     (try)
import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as BC (takeWhile)
import qualified Data.ByteString.Lazy  as BL (ByteString)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T (unpack)

import qualified Data.CaseInsensitive as CI (mk)
import Network.HTTP.Client
    ( HttpException
    , httpLbs
    , parseUrlThrow
    , responseBody
    , responseHeaders
    )
import Network.HTTP.Client.TLS
    ( getGlobalManager
    )

import Graze.HttpUrl (HttpUrl (..), serializeUrl)
import Graze.Robots  (Robots, parseRobots)

-- ContentType -----------------------------------------------------------------

data ContentType
    = TextHtml
    | TextPlain
    | Other

fromByteString :: B.ByteString -> Maybe ContentType
fromByteString s = case CI.mk (BC.takeWhile (/= ';') s) of
    "text/html"  -> Just TextHtml
    "text/plain" -> Just TextPlain
    _            -> Nothing

-- get -------------------------------------------------------------------------

type Result = (ContentType, BL.ByteString)

get :: HttpUrl -> IO Result
get url = do
    request  <- parseUrlThrow . T.unpack . serializeUrl $ url
    manager  <- getGlobalManager
    response <- httpLbs request manager
    let contentType = response & responseHeaders & lookup "Content-Type"
            >>= fromByteString & fromMaybe Other
    return (contentType, responseBody response)

-- getRobots -------------------------------------------------------------------

getRobots :: HttpUrl -> IO Robots
getRobots url = (try (get url') :: IO (Either HttpException Result)) <&> \case
    Right (TextPlain, s) -> parseRobots "graze" s
    _                    -> const True
  where
    url' = url {path = "/robots.txt"}
