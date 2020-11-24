{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( runFetcher
    ) where

import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TQueue  (readTQueue, writeTQueue)
import Control.Concurrent.STM.TBQueue (writeTBQueue)
import Control.Exception              (try)

import qualified Network.HTTP.Client as H (HttpException)

import Graze.Http
import Graze.Links
import Graze.Types
import Graze.Url


-- | Completes 'Job's from the shared 'fetcherQueue'.
--
-- A unit of work for a fetcher consists of downloading a page and parsing it;
-- the fetcher then passes the result to the writer, logger, and crawler.
runFetcher :: Queues -> IO ()
runFetcher Queues {..} = loop
  where
    loop = (atomically . readTQueue $ fetcherQueue) >>= \case
        StopFetching         -> atomically $
            writeTQueue fetcherQueue StopFetching
        Fetch job @ Job {..} -> do
            response :: Either H.HttpException Response <- try $ get url
            case response of
                Left _                  -> atomically $
                    writeTBQueue resultQueue Failure
                Right (contentType, bs) -> do
                    let links = case contentType of
                            TextHtml -> parseLinks url bs
                            _        -> []
                    atomically . writeTBQueue writerQueue $
                        Write (Record origin url links) bs
                    atomically . writeTBQueue loggerQueue $
                        Log ("Got " <> serializeUrl url)
                    atomically . writeTBQueue resultQueue $
                        Success job links
            loop
