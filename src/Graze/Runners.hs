{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Runners (Config (..), run) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import           Control.Monad                (replicateM_)
import qualified Data.ByteString.Char8        as B (writeFile)
import           Debug.Trace                  (traceIO)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))

import Graze.Crawler  (crawl, evalCrawler, initCrawler)
import Graze.HttpUrl  (HttpUrl (..))
import Graze.Messages (Job (..))
import Graze.Worker   (fetch)
import Graze.Writer   (evalWriter, write, WriterState (..))


data Config = Config
    { cWorkers  :: !Int       -- ^ Number of worker threads.
    , cDepth    :: !Int       -- ^ Depth of the search.
    , cFolder   :: !FilePath  -- ^ Folder to save the pages in.
    , cDatabase :: !FilePath  -- ^ Name of the resulting CSV.
    , cBase     :: !HttpUrl   -- ^ URL for the crawler to start at.
    }

run :: Config -> IO ()
run Config {..} = do
    traceIO $ "Crawling " <> show cBase

    jobChan <- newTChanIO
    resChan <- newTChanIO
    outChan <- newTChanIO
    atomically . writeTChan jobChan $ Job cDepth cBase cBase

    replicateM_ cWorkers . forkIO $ fetch jobChan resChan

    cs <- initCrawler cBase
    _  <- forkIO $ evalCrawler (crawl jobChan resChan outChan) cs

    createDirectoryIfMissing True cFolder
    B.writeFile (cFolder </> cDatabase) "id,parent,url\r\n"

    evalWriter (write outChan) $ WriterState 0 cFolder cDatabase
