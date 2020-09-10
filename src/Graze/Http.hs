{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- | Module: Graze.Http
--
-- Functions for making GET requests, based on "Network.HTTP.Client".
--------------------------------------------------------------------------------

module Graze.Http
    ( ContentType (..)
    , Result
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

-- | A partial representation of the \"Content-Type\" response header.
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

-- | The 'ContentType' of the response and its body.
type Result = (ContentType, BL.ByteString)

-- | Sends a GET request for the given URL and returns the result.
--
-- Throws 'HttpException' if the URL is invalid or the request is unsuccessful.
get :: HttpUrl -> IO Result
get url = do
    request  <- parseUrlThrow . T.unpack . serializeUrl $ url
    manager  <- getGlobalManager
    response <- httpLbs request manager
    let contentType = response & responseHeaders & lookup "Content-Type"
            >>= fromByteString & fromMaybe Other
    return (contentType, responseBody response)

-- getRobots -------------------------------------------------------------------

-- | Sends a GET request for the robots.txt file of the given URL's host and
-- returns the parsed result.
--
-- Throws 'HttpException' if the URL is invalid or the request is unsuccessful.
getRobots :: HttpUrl -> IO Robots
getRobots url = (try (get url') :: IO (Either HttpException Result)) <&> \case
    Right (TextPlain, s) -> parseRobots "graze" s
    _                    -> const True
  where
    url' = url {path = "/robots.txt"}
