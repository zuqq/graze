{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client".

module Graze.Http
    ( ContentType (..)
    , Result
    , get
    ) where

import           Control.Monad         ((<=<))
import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as BC (takeWhile)
import qualified Data.ByteString.Lazy  as BL (ByteString)
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T (unpack)

import qualified Data.CaseInsensitive    as CI (mk)
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H (getGlobalManager)

import Graze.Url (Url (..), serializeUrl)


-- | A partial representation of the \"Content-Type\" response header.
data ContentType
    = TextHtml
    | TextPlain
    | Other

-- | This returns a 'Maybe' even though we could just use 'Other' because that
-- makes it more easily composable.
fromByteString :: B.ByteString -> Maybe ContentType
fromByteString bs = case CI.mk (BC.takeWhile (/= ';') bs) of
    "text/html"  -> Just TextHtml
    "text/plain" -> Just TextPlain
    _            -> Nothing

fromResponse :: H.Response body -> ContentType
fromResponse = fromMaybe Other
    . (fromByteString <=< lookup "Content-Type")
    . H.responseHeaders

-- | The 'ContentType' of the response and its body.
type Result = (ContentType, BL.ByteString)

-- | Set the \"User-Agent\", since some sites will not serve us otherwise.
setUserAgent :: H.Request -> H.Request
setUserAgent request = request {H.requestHeaders = [("User-Agent", "graze")]}

-- | Sends a GET request for the given URL and returns the result.
--
-- Throws 'HttpException' if the URL is invalid or the request is unsuccessful.
get :: Url -> IO Result
get (T.unpack . serializeUrl -> url) = do
    request  <- setUserAgent <$> H.parseUrlThrow url
    manager  <- H.getGlobalManager
    response <- H.httpLbs request manager
    return (fromResponse response, H.responseBody response)
