{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( Chans (Chans)
    , run
    ) where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Exception            (try)

import Network.HTTP.Client (HttpException)

import Graze.Http     (ContentType (TextHtml), get)
import Graze.HttpUrl  (serializeUrl)
import Graze.Links    (parseLinks)
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
        StopFetching         -> return ()
        Fetch job @ Job {..} -> do
            try (get url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTChan outbox Failure
                Right (contentType, body) ->
                    let links = case contentType of
                            TextHtml -> parseLinks url body
                            _        -> []
                    in atomically $ do
                        writeTChan outbox $ Success job links body
                        writeTChan logger $ Log ("Got " <> serializeUrl url)
            loop
