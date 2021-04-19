{-# LANGUAGE OverloadedStrings #-}

-- | Functions for making GET requests, based on "Network.HTTP.Client".
module Graze.Http
    (
    -- Performing requests
      get
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

addRequestHeader :: CI ByteString -> ByteString -> Request -> Request
addRequestHeader key value request =
    request {requestHeaders = (key, value) : requestHeaders request}

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

get :: MediaType -> ByteString -> URI -> IO (Maybe Text)
get expected userAgent uri = do
    request <-
            -- Some hosts will not serve us without a "User-Agent".
            addRequestHeader "User-Agent" userAgent
        .   addRequestHeader "Accept" (renderHeader expected)
        <$> requestFromURI uri
    manager <- getGlobalManager
    parseResponse expected <$> httpLbs request manager
