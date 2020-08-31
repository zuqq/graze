{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( runFetcher
    ) where

import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TQueue  (writeTQueue)
import Control.Concurrent.STM.TBQueue (readTBQueue, writeTBQueue)
import Control.Exception              (try)

import Network.HTTP.Client (HttpException)

import Graze.Http    (ContentType (TextHtml), get)
import Graze.HttpUrl (serializeUrl)
import Graze.Links   (parseLinks)
import Graze.Types


runFetcher :: Queues -> IO ()
runFetcher Queues {..} = loop
  where
    loop = (atomically . readTBQueue $ fetcherQueue) >>= \case
        StopFetching         -> return ()
        Fetch job @ Job {..} -> do
            try (get url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTQueue resultQueue Failure
                Right (contentType, body) -> do
                    let links = case contentType of
                            TextHtml -> parseLinks url body
                            _        -> []
                    atomically . writeTQueue resultQueue $
                        Success job links body
                    atomically . writeTBQueue loggerQueue $
                        Log ("Got " <> serializeUrl url)
            loop
