{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( runFetcher
    ) where

import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TQueue  (readTQueue)
import Control.Concurrent.STM.TBQueue (writeTBQueue)
import Control.Exception              (try)

import Network.HTTP.Client (HttpException)

import Graze.Http    (ContentType (TextHtml), get)
import Graze.HttpUrl (serializeUrl)
import Graze.Links   (parseLinks)
import Graze.Types


runFetcher :: Queues -> IO ()
runFetcher Queues {..} = loop
  where
    loop = (atomically . readTQueue $ fetcherQueue) >>= \case
        StopFetching         -> return ()
        Fetch job @ Job {..} -> do
            try (get url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTBQueue resultQueue Failure
                Right (contentType, body) -> do
                    let links = case contentType of
                            TextHtml -> parseLinks url body
                            _        -> []
                    atomically . writeTBQueue writerQueue $
                        Write (Record origin url links) body
                    atomically . writeTBQueue loggerQueue $
                        Log ("Got " <> serializeUrl url)
                    atomically . writeTBQueue resultQueue $
                        Success job links
            loop
