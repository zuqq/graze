{-# LANGUAGE OverloadedStrings #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client".
module Graze.Http
    ( ContentType (..)
    , HttpException
    , Response
    , get
    )
    where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (HttpException)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS

import Graze.URI

-- | A partial representation of the \"Content-Type\" response header.
data ContentType
    = TextHtml
    | TextPlain
    | Other

-- | This returns a 'Maybe' even though we could just use 'Other' because that
-- makes it more easily composable.
parseContentType :: ByteString -> Maybe ContentType
parseContentType bs = case CI.mk . Char8.takeWhile (/= ';') $ bs of
    "text/html"  -> Just TextHtml
    "text/plain" -> Just TextPlain
    _            -> Nothing

extractContentType :: Http.Response body -> ContentType
extractContentType = fromMaybe Other
    . (parseContentType <=< lookup "Content-Type")
    . Http.responseHeaders

-- | The 'ContentType' of the response and its body.
type Response = (ContentType, Lazy.ByteString)

-- | Set the \"User-Agent\", since some sites will not serve us otherwise.
setUserAgent :: Http.Request -> Http.Request
setUserAgent request = request {Http.requestHeaders = [("User-Agent", "graze")]}

-- | Sends a GET request for the given URL and returns the response.
--
-- Throws 'HttpException' if the given URL is invalid or the request was
-- unsuccessful.
get :: URI -> IO Response
get uri = do
    request  <- setUserAgent <$> Http.requestFromURI uri
    manager  <- TLS.getGlobalManager
    response <- Http.httpLbs request manager
    pure (extractContentType response, Http.responseBody response)
