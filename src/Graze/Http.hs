{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client".

module Graze.Http
    ( ContentType (..)
    , Response
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

import Graze.Url


-- | A partial representation of the \"Content-Type\" response header.
data ContentType
    = TextHtml
    | TextPlain
    | Other

-- | This returns a 'Maybe' even though we could just use 'Other' because that
-- makes it more easily composable.
parseContentType :: B.ByteString -> Maybe ContentType
parseContentType bs = case CI.mk . BC.takeWhile (/= ';') $ bs of
    "text/html"  -> Just TextHtml
    "text/plain" -> Just TextPlain
    _            -> Nothing

extractContentType :: H.Response body -> ContentType
extractContentType = fromMaybe Other
    . (parseContentType <=< lookup "Content-Type")
    . H.responseHeaders

-- | The 'ContentType' of the response and its body.
type Response = (ContentType, BL.ByteString)

-- | Set the \"User-Agent\", since some sites will not serve us otherwise.
setUserAgent :: H.Request -> H.Request
setUserAgent request = request {H.requestHeaders = [("User-Agent", "graze")]}

-- | Sends a GET request for the given URL and returns the response.
--
-- Throws 'H.HttpException' if the given URL is invalid or the request was
-- unsuccessful.
get :: Url -> IO Response
get (T.unpack . serializeUrl -> url) = do
    request  <- setUserAgent <$> H.parseUrlThrow url
    manager  <- H.getGlobalManager
    response <- H.httpLbs request manager
    return (extractContentType response, H.responseBody response)
