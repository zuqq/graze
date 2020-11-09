{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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

import           Control.Monad         ((<=<))
import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as BC (takeWhile)
import qualified Data.ByteString.Lazy  as BL (ByteString)
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T (unpack)

import qualified Data.CaseInsensitive    as CI (mk)
import           Network.HTTP.Client     hiding (path)
import           Network.HTTP.Client.TLS (getGlobalManager)

import Graze.HttpUrl (HttpUrl (..), serializeUrl)
import Graze.Robots  (Robots, parseRobots)


-- ContentType -----------------------------------------------------------------

-- | A partial representation of the \"Content-Type\" response header.
data ContentType
    = TextHtml
    | TextPlain
    | Other

-- | This returns a 'Maybe' even though we could just use 'Other' because that
-- makes it more easily composable.
fromByteString :: B.ByteString -> Maybe ContentType
fromByteString s = case CI.mk (BC.takeWhile (/= ';') s) of
    "text/html"  -> Just TextHtml
    "text/plain" -> Just TextPlain
    _            -> Nothing

fromResponse :: Response body -> ContentType
fromResponse = fromMaybe Other
    . (fromByteString <=< lookup "Content-Type")
    . responseHeaders

-- get -------------------------------------------------------------------------

-- | The 'ContentType' of the response and its body.
type Result = (ContentType, BL.ByteString)

-- | Set the \"User-Agent\", since some sites will not serve us otherwise.
setUserAgent :: Request -> Request
setUserAgent request = request {requestHeaders = [("User-Agent", "graze")]}

-- | Sends a GET request for the given URL and returns the result.
--
-- Throws 'HttpException' if the URL is invalid or the request is unsuccessful.
get :: HttpUrl -> IO Result
get (T.unpack . serializeUrl -> url) = do
    request  <- setUserAgent <$> parseUrlThrow url
    manager  <- getGlobalManager
    response <- httpLbs request manager
    return (fromResponse response, responseBody response)

-- getRobots -------------------------------------------------------------------

-- | Sends a GET request for the robots.txt file of the given URL's host and
-- returns the parsed result.
--
-- Throws 'HttpException' if the URL is invalid or the request is unsuccessful.
getRobots :: HttpUrl -> IO Robots
getRobots url = get url {path = "/robots.txt"} <&> \case
    (TextPlain, s) -> parseRobots "graze" s
    _              -> const True
