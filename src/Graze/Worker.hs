module Graze.Worker (fetch) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan, TChan, writeTChan)
import           Control.Exception            (try)
import qualified Data.ByteString.Char8        as B (ByteString)
import           Debug.Trace                  (traceIO)

import Network.HTTP.Conduit (HttpException)

import Graze.Http     (reqPage)
import Graze.Messages (FetchResponse (..), Job (..), Result (..))


fetch
    :: TChan Job
    -> TChan FetchResponse
    -> IO ()
fetch jobChan resChan = loop
  where
    loop = do
        job <- atomically (readTChan jobChan)
        traceIO $ "[" <> show (jHops job) <> "] " <> "GET " <> show (jUrl job)
        resp <- try (reqPage . jUrl $ job)
                :: IO (Either HttpException B.ByteString)
        atomically . writeTChan resChan . FetchResponse job $
            case resp of
                Left _     -> Fail
                Right page -> Success page
        loop
