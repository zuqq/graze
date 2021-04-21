{-# LANGUAGE OverloadedStrings #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client" and
-- "Network.HTTP.Media".
module Graze.Http
    ( get
    -- * Reexports
    , (//)
    , HttpException
    )
    where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.List (uncons)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text

import Graze.URI

parseContentType :: ByteString -> Maybe MediaType
parseContentType header =
        parseAccept header
    >>= matchContent ["text" // "html", "text" // "plain"]

decodeLatin1 :: Lazy.ByteString -> Maybe Text
decodeLatin1 = Just . Text.decodeLatin1 . Lazy.toStrict

decodeUtf8 :: Lazy.ByteString -> Maybe Text
decodeUtf8 bs =
    case Text.decodeUtf8' (Lazy.toStrict bs) of
        Left _  -> Nothing
        Right s -> Just s

parseCharset :: ByteString -> Maybe (Lazy.ByteString -> Maybe Text)
parseCharset = mapContentCharset [("iso-8859-1", decodeLatin1)]

addRequestHeaders :: [(CI ByteString, ByteString)] -> Request -> Request
addRequestHeaders headers request =
    request {requestHeaders = headers <> requestHeaders request}

getResponseHeader :: CI ByteString -> Response a -> [ByteString]
getResponseHeader name response =
    [value | (key, value) <- responseHeaders response, key == name]

parseResponse :: MediaType -> Response Lazy.ByteString -> Maybe Text
parseResponse expected response = do
    -- There should be at most one "Content-Type" header, so we take the first
    -- one (if it exists). Response bodies with no "Content-Type" are ignored.
    (header, _) <- uncons (getResponseHeader "Content-Type" response)
    mediaType <- parseContentType header
    decode <-
            parseCharset header
            -- Default to UTF-8, which also covers US-ASCII.
        <|> Just decodeUtf8
    if mediaType == expected
        then decode (responseBody response)
        else Nothing

-- | Send a GET request with custom @Accept@ and @UserAgent@ headers.
--
-- Throws 'HttpException' if the given 'URI' is invalid or the request failed.
--
-- Returns 'Nothing' if the response is of the wrong 'MediaType' or encoding.
get
    :: MediaType
    -> ByteString  -- ^ User agent.
    -> URI
    -> IO (Maybe Text)
get expected userAgent uri = do
    request <-
            addRequestHeaders
                -- Add "Accept" in order to be a good citizen of the WWW; add
                -- "User-Agent" because some hosts will not serve us otherwise.
                [("Accept", renderHeader expected), ("User-Agent", userAgent)]
        <$> requestFromURI uri
    manager <- getGlobalManager
    parseResponse expected <$> httpLbs request manager
