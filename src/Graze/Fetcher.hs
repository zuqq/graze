{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( runFetcher
    ) where

import Control.Concurrent.STM (STM, atomically)
import Control.Exception              (try)
import qualified Data.Text as T (Text)

import qualified Network.HTTP.Client as H (HttpException)

import Graze.Http
import Graze.Links
import Graze.Types
import Graze.Url


-- | Completes 'Job's.
--
-- A unit of work for a fetcher consists of downloading a page and parsing it;
-- the fetcher then passes the result to the writer, logger, and crawler.
runFetcher
    :: STM (Maybe Job)     -- ^ Receive a 'Job'.
    -> (Write -> STM ())   -- ^ Send a 'Write' to the writer.
    -> (T.Text -> STM ())  -- ^ Send a 'T.Text' to the logger.
    -> (Result -> STM ())  -- ^ Send a 'Result' to the crawler.
    -> IO ()
runFetcher recv sendWriter sendLogger sendCrawler = loop
  where
    loop = atomically recv >>= \case
        Nothing             -> return ()
        Just job @ Job {..} -> do
            response :: Either H.HttpException Response <- try $ get url
            case response of
                Left _                  -> atomically $ sendCrawler Failure
                Right (contentType, bs) -> do
                    let links = case contentType of
                            TextHtml -> parseLinks url bs
                            _        -> []
                    atomically . sendWriter $ Write (Record origin url links) bs
                    atomically . sendLogger $ "Got " <> serializeUrl url
                    atomically . sendCrawler $ Success job links
            loop
