{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Runners
    ( Config (..)
    , run
    ) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import           Control.Monad                (replicateM_)
import qualified Data.ByteString              as B (writeFile)
import qualified Data.Set                     as S (singleton)
import           Debug.Trace                  (traceIO)
import           System.Directory             (createDirectoryIfMissing)

import Network.HTTP.Client.TLS (newTlsManager, setGlobalManager)

import Graze.Crawler  (CrawlerState (..), crawl, evalCrawler)
import Graze.Http     (robots)
import Graze.HttpUrl  (HttpUrl (..), serialize)
import Graze.Messages (Job (..))
import Graze.Robots   (allowed)
import Graze.Worker   (fetch)
import Graze.Writer   (write)


data Config = Config
    { cWorkers :: !Int       -- ^ Number of worker threads.
    , cDepth   :: !Int       -- ^ Depth of the search.
    , cFolder  :: !FilePath  -- ^ Download folder.
    , cRecords :: !FilePath  -- ^ Page record file.
    , cBase    :: !HttpUrl   -- ^ URL to start at.
    }

run :: Config -> IO ()
run Config {..} = do
    traceIO $ "Crawling " <> (show . serialize) cBase

    jobChan <- newTChanIO
    repChan <- newTChanIO
    outChan <- newTChanIO

    atomically $
        writeTChan jobChan (Job cDepth cBase cBase)

    m <- newTlsManager
    setGlobalManager m

    replicateM_ cWorkers $
        forkIO (fetch jobChan repChan)

    rs <- robots cBase
    _  <- forkIO $
            evalCrawler
                (crawl (allowed cBase rs) jobChan repChan outChan)
                (CrawlerState 1 (S.singleton cBase))

    createDirectoryIfMissing True cFolder
    B.writeFile cRecords ""

    write cFolder cRecords outChan
