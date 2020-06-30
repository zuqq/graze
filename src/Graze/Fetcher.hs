{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( Chans (Chans)
    , run
    ) where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Exception            (try)
import Data.Time.LocalTime          (getZonedTime)

import Network.HTTP.Client (HttpException)

import Graze.Http     (request)
import Graze.HttpUrl  (serialize)
import Graze.Links    (links)
import Graze.Messages


data Chans = Chans
    { inbox  :: !(TChan FetchCommand)
    , outbox :: !(TChan FetchResult)
    , logger :: !(TChan LogCommand)
    }

run :: Chans -> IO ()
run Chans {..} = loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopFetching -> return ()
        Fetch job    -> do
            t <- getZonedTime
            let url = jUrl job
            atomically $
                writeTChan logger $
                    Log (Message t Debug (serialize url))
            try (request url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTChan outbox Failure
                Right content             -> atomically $
                    writeTChan outbox $
                        Success (Record job (links url content) content)
            loop
