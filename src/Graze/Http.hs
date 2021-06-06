{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client",
-- "Network.HTTP.Media", and "Network.HTTP.Types".
--
-- Because this module uses 'Network.HTTP.Client.TLS.getGlobalManager', it
-- supports HTTPS out of the box.
module Graze.Http
    ( GrazeHttpException (..)
    , getText
    -- * Reexports
    , HttpException (..)
    , UnicodeException (..)
    )
    where

import Control.Exception (Exception, catch, throwIO)
import Data.ByteString (ByteString)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media
import Network.HTTP.Types

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text

import Graze.URI

data GrazeHttpException
    = RequestException !HttpException
    | ContentException !(Maybe ByteString)
    | DecodingException !UnicodeException
    deriving Show

instance Exception GrazeHttpException

wrapHttpException :: IO a -> IO a
wrapHttpException m = catch @HttpException m (throwIO . RequestException)

wrapUnicodeException :: Either UnicodeException a -> IO a
wrapUnicodeException = either (throwIO . DecodingException) pure

decodeLatin1 :: Lazy.ByteString -> Either UnicodeException Text
decodeLatin1 = Right . Text.decodeLatin1 . Lazy.toStrict

decodeUtf8 :: Lazy.ByteString -> Either UnicodeException Text
decodeUtf8 = Text.decodeUtf8' . Lazy.toStrict

decode :: ByteString -> Lazy.ByteString -> Either UnicodeException Text
decode = fromMaybe decodeUtf8 . mapContentCharset [("iso-8859-1", decodeLatin1)]

addRequestHeader :: Header -> Request -> Request
addRequestHeader header request =
    request {requestHeaders = header : requestHeaders request}

getResponseHeader :: HeaderName -> Response a -> [ByteString]
getResponseHeader name response =
    [value | (key, value) <- responseHeaders response, key == name]

requestFromURI' :: URI -> IO Request
requestFromURI' uri = do
    request <- wrapHttpException (requestFromURI uri)
    -- See 'parseUrlThrow'.
    pure (request {checkResponse = throwErrorStatusCodes})

httpLbs' :: Request -> Manager -> IO (Response Lazy.ByteString)
httpLbs' request manager = do
    -- We need to set the @User-Agent@ header because some hosts will not serve
    -- us otherwise.
    let request' = addRequestHeader (hUserAgent, "graze") request
    wrapHttpException (httpLbs request' manager)

getContentType :: Response a -> IO ByteString
getContentType response =
    -- There should be at most one @Content-Type@ response header, so we take
    -- the first one (if it exists).
    case uncons (getResponseHeader hContentType response) of
        Nothing -> throwIO (ContentException Nothing)
        Just (contentType, _) -> pure contentType

-- | Request the given 'URI' and decode the response.
--
-- Throws 'GrazeHttpException' if the request fails.
getText :: URI -> IO Text
getText uri = do
    request <- requestFromURI' uri
    manager <- getGlobalManager
    response <- httpLbs' request manager
    contentType <- getContentType response
    case matchContent ["text" // "html", "text" // "plain"] contentType of
        Nothing -> throwIO (ContentException (Just contentType))
        Just _ ->
            wrapUnicodeException (decode contentType (responseBody response))
