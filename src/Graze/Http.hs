{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client",
-- "Network.HTTP.Media", and "Network.HTTP.Types".
module Graze.Http
    ( GrazeHttpException (..)
    , get
    -- * Reexports
    , (//)
    , HttpException (..)
    , Text.UnicodeException (..)
    )
    where

import Control.Exception (Exception, catch, throwIO)
import Data.ByteString (ByteString)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media
import Network.HTTP.Types

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import Graze.URI

parseContentType :: ByteString -> Maybe MediaType
parseContentType header =
        parseAccept header
    >>= matchContent ["text" // "html", "text" // "plain"]

type Decoder = Lazy.ByteString -> Either Text.UnicodeException Text

decodeLatin1 :: Decoder
decodeLatin1 = Right . Text.decodeLatin1 . Lazy.toStrict

decodeUtf8 :: Decoder
decodeUtf8 = Text.decodeUtf8' . Lazy.toStrict

parseCharset :: ByteString -> Maybe Decoder
parseCharset = mapContentCharset [("iso-8859-1", decodeLatin1)]

addRequestHeaders :: RequestHeaders -> Request -> Request
addRequestHeaders headers request =
    request {requestHeaders = headers <> requestHeaders request}

getResponseHeader :: HeaderName -> Response a -> [ByteString]
getResponseHeader name response =
    [value | (key, value) <- responseHeaders response, key == name]

data GrazeHttpException
    = RequestException !HttpException
    | NoContentType   -- ^ No @Content-Type@ response header.
    | WrongMediaType  -- ^ Received the wrong media type.
        !MediaType    -- ^ Expected media type.
        !ByteString   -- ^ Value of the @Content-Type@ response header.
    | DecodingException !Text.UnicodeException
    -- ^ The media type in the @Content-Type@ header is not Latin-1 and the body
    -- is not valid UTF-8.

instance Exception GrazeHttpException

instance Show GrazeHttpException where
    show (RequestException e)             = show e
    show NoContentType                    = "No Content-Type response header."
    show (WrongMediaType expected header) =
            "Expected "
        <>  show expected
        <>  " but got the non-matching "
        <>  show header
        <>  "."
    show (DecodingException e)            = show e

wrapHttpException :: IO a -> IO a
wrapHttpException m = catch @HttpException m (throwIO . RequestException)

wrapUnicodeException :: Either Text.UnicodeException a -> IO a
wrapUnicodeException = either (throwIO . DecodingException) pure

-- | Send a GET request with custom @Accept@ and @User-Agent@ request headers.
--
-- Throws 'GrazeHttpException' if something goes wrong.
get
    :: MediaType   -- ^ Expected media type.
    -> ByteString  -- ^ User agent.
    -> URI         -- ^ URL to request.
    -> IO Text
get expected userAgent uri = do
    -- Note that we need to set the @User-Agent@ header because some hosts will
    -- not serve us otherwise.
    let headers = [(hAccept, renderHeader expected), (hUserAgent, userAgent)]
    response <- wrapHttpException (do
        request <-
                setRequestCheckStatus . addRequestHeaders headers
            <$> requestFromURI uri
        manager <- getGlobalManager
        httpLbs request manager)

    -- There should be at most one @Content-Type@ response header, so we take
    -- the first one (if it exists). Responses with no @Content-Type@ cause a
    -- 'NoContentType' exception.
    header <-
        case uncons (getResponseHeader hContentType response) of
            Nothing -> throwIO NoContentType
            Just (header, _) -> pure header

    case parseContentType header of
        Just mediaType
            | mediaType == expected -> pure ()
        _ -> throwIO (WrongMediaType expected header)

    let decode = fromMaybe decodeUtf8 (parseCharset header)
    wrapUnicodeException (decode (responseBody response))
