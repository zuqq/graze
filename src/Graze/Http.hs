{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client",
-- "Network.HTTP.Media", and "Network.HTTP.Types".
--
-- Because this module uses 'Network.HTTP.Client.TLS.getGlobalManager', it
-- supports HTTPS out of the box.
module Graze.Http
    ( GrazeHttpException (..)
    , getTextHtml
    , getTextPlain
    -- * Reexports
    , HttpException (..)
    , UnicodeException (..)
    )
    where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad ((>=>))
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

textHtml :: MediaType
textHtml = "text" // "html"

textPlain :: MediaType
textPlain = "text" // "plain"

parseContentType :: ByteString -> Maybe MediaType
parseContentType header =
    parseAccept header >>= matchContent [textHtml, textPlain]

type Decoder = Lazy.ByteString -> Either UnicodeException Text

decodeLatin1 :: Decoder
decodeLatin1 = Right . Text.decodeLatin1 . Lazy.toStrict

decodeUtf8 :: Decoder
decodeUtf8 = Text.decodeUtf8' . Lazy.toStrict

parseCharset :: ByteString -> Maybe Decoder
parseCharset = mapContentCharset [("iso-8859-1", decodeLatin1)]

addRequestHeader :: Header -> Request -> Request
addRequestHeader header request =
    request {requestHeaders = header : requestHeaders request}

getResponseHeader :: HeaderName -> Response a -> [ByteString]
getResponseHeader name response =
    [value | (key, value) <- responseHeaders response, key == name]

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

requestFromURI' :: URI -> IO Request
requestFromURI' = wrapHttpException . requestFromURI

httpLbs' :: Request -> IO (Response Lazy.ByteString)
httpLbs' request = getGlobalManager >>= wrapHttpException . httpLbs request'
  where
    -- Note that we need to set the @User-Agent@ header because some hosts will
    -- not serve us otherwise.
    request' = addRequestHeader (hUserAgent, "graze") request

getContentType :: Response a -> IO ByteString
getContentType response =
    -- There should be at most one @Content-Type@ response header, so we take
    -- the first one (if it exists). Responses with no @Content-Type@ cause a
    -- 'NoContentType' exception.
    case uncons (getResponseHeader hContentType response) of
        Nothing -> throwIO NoContentType
        Just (header, _) -> pure header

getOnly :: MediaType -> URI -> IO (Response Lazy.ByteString)
getOnly accept uri = do
    request <- requestFromURI' uri
    let request' = addRequestHeader (hAccept, renderHeader accept) request
    response <- httpLbs' request'
    contentType <- getContentType response
    case parseContentType contentType of
        Just mediaType
            | mediaType == accept -> pure response
        _ -> throwIO (WrongMediaType accept contentType)

decodeResponse ::  Response Lazy.ByteString -> IO Text
decodeResponse response = do
    contentType <- getContentType response
    let decode = fromMaybe decodeUtf8 (parseCharset contentType)
    wrapUnicodeException (decode (responseBody response))

-- | Send a GET request for the given 'URI', accepting only @text/html@.
--
-- Throws 'GrazeHttpException'.
getTextHtml :: URI -> IO Text
getTextHtml = getOnly textHtml >=> decodeResponse

-- | Send a GET request for the given 'URI', accepting only @text/plain@.
--
-- Throws 'GrazeHttpException'.
getTextPlain :: URI -> IO Text
getTextPlain = getOnly textPlain >=> decodeResponse
