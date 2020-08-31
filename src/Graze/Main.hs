{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Main
    ( Config (..)
    , runMain
    ) where

import           Control.Concurrent             (forkFinally)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TQueue  (newTQueueIO)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import           Control.Concurrent.STM.TMVar
import           Control.Monad                  (replicateM, replicateM_)
import           Data.Foldable                  (traverse_)
import qualified Data.Text                      as T (unpack)

import Network.HTTP.Client.TLS (newTlsManager, setGlobalManager)

import Graze.Crawler (CrawlerConfig (..), runCrawler)
import Graze.Fetcher (runFetcher)
import Graze.Http    (getRobots)
import Graze.HttpUrl (HttpUrl (..), serializeUrl)
import Graze.Logger  (runLogger)
import Graze.Types
import Graze.Writer  (runWriter)


data Config = Config
    { base    :: !HttpUrl   -- ^ URL to start at.
    , folder  :: !FilePath  -- ^ Download folder.
    , depth   :: !Int       -- ^ Depth of the search.
    , threads :: !Int       -- ^ Number of threads.
    }

runMain :: Config -> IO ()
runMain Config {..} = do
    putStrLn $ "Crawling " <> T.unpack (serializeUrl base)

    tls <- newTlsManager
    setGlobalManager tls

    let n = fromIntegral threads
    fetcherQueue <- newTBQueueIO n
    writerQueue  <- newTBQueueIO n
    loggerQueue  <- newTBQueueIO n
    resultQueue  <- newTQueueIO

    let queues = Queues {..}

    let forkChild x = do
            m <- atomically newEmptyTMVar
            _ <- forkFinally x (\_ -> atomically $ putTMVar m ())
            return m

    lm <- forkChild $ runLogger queues

    wm <- forkChild $ runWriter folder queues

    ms <- replicateM threads . forkChild $ runFetcher queues

    p <- getRobots base
    let legal url = domain url == domain base && p (path url)
    runCrawler CrawlerConfig {..} queues

    atomically $ do
        replicateM_ threads $ writeTBQueue fetcherQueue StopFetching
        writeTBQueue writerQueue StopWriting
        writeTBQueue loggerQueue StopLogging

    traverse_ (atomically . takeTMVar) (lm : wm : ms)

    putStrLn "Done"
