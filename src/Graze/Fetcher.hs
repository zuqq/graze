{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( Chans (Chans)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import           Control.Exception            (try)
import qualified Data.ByteString.Lazy         as BL (toStrict)

import Network.HTTP.Client (HttpException)

import Graze.Http     (ContentType (Html), get)
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
            try (get url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTChan outbox Failure
                Right (contentType, body) ->
                    let ls = case contentType of
                            Html -> links url . BL.toStrict $ body
                            _    -> []
                    in atomically $ do
                        writeTChan outbox $ Success (Record job ls body)
                        writeTChan logger $ Log ("Got " <> serialize url)
            loop
