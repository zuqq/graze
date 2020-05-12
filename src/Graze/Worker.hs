{-# LANGUAGE RecordWildCards #-}

module Graze.Worker
    ( fetch
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan, TChan, writeTChan)
import           Control.Exception            (try)
import qualified Data.ByteString              as B (ByteString)
import           Debug.Trace                  (traceIO)

import Network.HTTP.Conduit (HttpException)

import Graze.Http     (reqPage)
import Graze.HttpUrl  (serialize)
import Graze.Messages (FetchResponse (..), Job (..), Result (..))


fetch
    :: TChan Job
    -> TChan FetchResponse
    -> IO ()
fetch jobChan resChan = loop
  where
    loop = do
        job@Job {..} <- atomically (readTChan jobChan)
        traceIO $
            "[" <> show jDepth <> "] " <> "GET " <> (show . serialize) jUrl
        resp <- try (reqPage jUrl)
                :: IO (Either HttpException B.ByteString)
        atomically . writeTChan resChan . FetchResponse job $
            case resp of
                Left _     -> Fail
                Right page -> Success page
        loop
