{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( runFetcher
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TQueue  (readTQueue)
import           Control.Concurrent.STM.TBQueue (writeTBQueue)
import           Control.Exception              (try)
import qualified Data.ByteString.Lazy           as BL (toStrict)
import qualified Data.Text.Encoding             as T (decodeUtf8')

import qualified Network.HTTP.Client as H (HttpException)

import Graze.Http
import Graze.Links
import Graze.Types
import Graze.Url


runFetcher :: Queues -> IO ()
runFetcher Queues {..} = loop
  where
    loop = (atomically . readTQueue $ fetcherQueue) >>= \case
        StopFetching         -> return ()
        Fetch job @ Job {..} -> do
            try (get url) >>= \case
                Left (_ :: H.HttpException) -> atomically $
                    writeTBQueue resultQueue Failure
                Right (contentType, bs)     -> do
                    let links = case contentType of
                            TextHtml -> case T.decodeUtf8' . BL.toStrict $ bs of
                                Left _  -> []
                                Right s -> parseLinks url s
                            _        -> []
                    atomically . writeTBQueue writerQueue $
                        Write (Record origin url links) bs
                    atomically . writeTBQueue loggerQueue $
                        Log ("Got " <> serializeUrl url)
                    atomically . writeTBQueue resultQueue $
                        Success job links
            loop
