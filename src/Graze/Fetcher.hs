{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( Chans (Chans)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import           Control.Exception            (try)
import qualified Data.ByteString.Lazy         as L (toStrict)
import           Data.Time.LocalTime          (getZonedTime)

import Network.HTTP.Client (HttpException)

import Graze.Http     (ContentType (TextHtml), get)
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
            let url = jUrl job
            t <- getZonedTime
            atomically $
                writeTChan logger $
                    Log (Message t Debug (serialize url))
            try (get url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTChan outbox Failure
                Right (contentType, body) ->
                    let ls = case contentType of
                            TextHtml -> links url . L.toStrict $ body
                            _        -> []
                    in atomically $
                        writeTChan outbox $
                            Success (Record job ls body)
            loop
