module Graze.Worker (fetch) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan, writeTChan)
import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import Debug.Trace (traceIO)

import Network.HTTP.Conduit (HttpException)

import Graze.Http (reqPage)
import Graze.Messages (Result(..), FetchResponse(..), Job(..))


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
                :: IO (Either HttpException BL.ByteString)
        atomically . writeTChan resChan . FetchResponse job $
            case resp of
                Left _     -> Fail
                Right page -> Success page
        loop
