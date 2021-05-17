{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client",
-- "Network.HTTP.Media", and "Network.HTTP.Types".
--
-- Because this module uses 'Network.HTTP.Client.TLS.getGlobalManager', it
-- supports HTTPS out of the box.
module Graze.Http
    ( GrazeHttpException (..)
    , decodeResponse
    , getOnly
    -- * Reexports
    , (//)
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

-- | HTTP request failure modes.
data GrazeHttpException
    = RequestException !HttpException
    | NoContentType   -- ^ No @Content-Type@ response header.
    | WrongMediaType  -- ^ Received the wrong media type.
        !MediaType    -- ^ Expected media type.
        !ByteString   -- ^ Value of the @Content-Type@ response header.
    | DecodingException !UnicodeException
    -- ^ The media type in the @Content-Type@ header is not Latin-1 and the body
    -- is not valid UTF-8.

instance Exception GrazeHttpException

instance Show GrazeHttpException where
    show (RequestException e)                = show e
    show NoContentType                       =
        "No Content-Type response header."
    show (WrongMediaType accept contentType) =
            "Expected "
        <>  show accept
        <>  " but got the non-matching "
        <>  show contentType
        <>  "."
    show (DecodingException e)               = show e

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

-- | Decode a 'Response'. The supported charsets are Latin-1 and anything that
-- is a subset of UTF-8.
--
-- Throws 'GrazeHttpException' if decoding fails.
decodeResponse :: Response Lazy.ByteString -> IO Text
decodeResponse response = do
    contentType <- getContentType response
    wrapUnicodeException (decode contentType (responseBody response))

addRequestHeader :: Header -> Request -> Request
addRequestHeader header request =
    request {requestHeaders = header : requestHeaders request}

getResponseHeader :: HeaderName -> Response a -> [ByteString]
getResponseHeader name response =
    [value | (key, value) <- responseHeaders response, key == name]

requestFromURI' :: URI -> IO Request
requestFromURI' = wrapHttpException . requestFromURI

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
        Nothing -> throwIO NoContentType
        Just (contentType, _) -> pure contentType

checkContentType :: MediaType -> Response a -> IO (Response a)
checkContentType accept response = do
    contentType <- getContentType response
    case matchContent [accept] contentType of
        Nothing -> throwIO (WrongMediaType accept contentType)
        Just _ -> pure response

-- | Request the given 'URI', setting the @Accept@ request header to the given
-- 'MediaType' and checking that the @Content-Type@ response header matches.
--
-- Throws 'GrazeHttpException' if the request fails.
getOnly :: MediaType -> URI -> IO (Response Lazy.ByteString)
getOnly accept uri = do
    request <- requestFromURI' uri
    let request' = addRequestHeader (hAccept, renderHeader accept) request
    manager <- getGlobalManager
    response <- httpLbs' request' manager
    checkContentType accept response
