{-# LANGUAGE RecordWildCards #-}

module Graze.Worker
    ( fetch
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import           Control.Exception            (try)
import qualified Data.ByteString              as B (ByteString)
import           Debug.Trace                  (traceIO)

import Network.HTTP.Conduit (HttpException)

import Graze.Http     (request)
import Graze.HttpUrl  (serialize)
import Graze.Messages (Report (..), Job (..), Result (..))


toResult :: Either e B.ByteString -> Result
toResult = either (const Fail) Success

fetch
    :: TChan Job
    -> TChan Report
    -> IO ()
fetch jobChan repChan = loop
  where
    loop = do
        job@Job {..} <- atomically (readTChan jobChan)
        traceIO . ("GET " <>) . show . serialize $ jUrl
        res <- try (request jUrl) :: IO (Either HttpException B.ByteString)
        atomically $
            writeTChan repChan (Report job (toResult res))
        loop
