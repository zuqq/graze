{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher
    ( runFetcher
    ) where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Exception            (try)

import Network.HTTP.Client (HttpException)

import Graze.Http    (ContentType (TextHtml), get)
import Graze.HttpUrl (serializeUrl)
import Graze.Links   (parseLinks)
import Graze.Types


runFetcher :: Chans -> IO ()
runFetcher Chans {..} = loop
  where
    loop = (atomically . readTChan $ fetcherChan) >>= \case
        StopFetching         -> return ()
        Fetch job @ Job {..} -> do
            try (get url) >>= \case
                Left (_ :: HttpException) -> atomically $
                    writeTChan resultChan Failure
                Right (contentType, body) ->
                    let links = case contentType of
                            TextHtml -> parseLinks url body
                            _        -> []
                    in atomically $ do
                        writeTChan resultChan $ Success job links body
                        writeTChan loggerChan $ Log ("Got " <> serializeUrl url)
            loop
