{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------
-- | Module: Graze
--
-- This module ties together the different components of the system.
--
-- Concurrency is achieved through the creation of multiple threads that
-- use queues to communicate. The main thread spawns
--
--     * a logger thread,
--     * a writer thread,
--     * and 'threads' fetcher threads.
--
-- The fetcher threads retrieve jobs from a shared job queue. A unit of work for
-- a fetcher thread consists of downloading a page and parsing it; the fetcher
-- thread then passes the result back to the main thread, where the page is
-- entered into the set of visited pages and new jobs are created from its
-- outgoing links. The fetcher thread also dispatches to the writer thread,
-- which writes the page and its metadata to the filesystem.
--------------------------------------------------------------------------------

module Graze
    ( Config (..)
    , run
    ) where

import           Control.Concurrent             (forkFinally)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TQueue  (newTQueueIO, writeTQueue)
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


-- | Configuration for the main thread.
data Config = Config
    { base    :: HttpUrl   -- ^ URL to start at.
    , folder  :: FilePath  -- ^ Download folder.
    , depth   :: Int       -- ^ Depth of the search.
    , threads :: Int       -- ^ Number of threads.
    }

-- | Run the main thread.
run :: Config -> IO ()
run Config {..} = do
    putStrLn $ "Crawling " <> T.unpack (serializeUrl base)

    tls <- newTlsManager
    setGlobalManager tls

    let n = fromIntegral threads
    fetcherQueue <- newTQueueIO
    -- These queues are bounded in order to provide sufficient backpressure.
    -- If any of them is full, all fetcher threads block. This gives the other
    -- threads a chance to catch up.
    writerQueue  <- newTBQueueIO n
    loggerQueue  <- newTBQueueIO n
    resultQueue  <- newTBQueueIO n
    let queues = Queues {..}

    let forkChild x = do
            m <- atomically newEmptyTMVar
            _ <- forkFinally x (\_ -> atomically $ putTMVar m ())
            return m

    lm <- forkChild $ runLogger queues

    wm <- forkChild $ runWriter folder queues

    ms <- replicateM threads . forkChild $ runFetcher queues

    p <- getRobots base
    -- Note that we only follow links that don't leave the domain.
    let legal url = domain url == domain base && p (path url)
    runCrawler CrawlerConfig {..} queues

    -- Tell the threads to shut down.
    atomically $ do
        replicateM_ threads $ writeTQueue fetcherQueue StopFetching
        writeTBQueue writerQueue StopWriting
        writeTBQueue loggerQueue StopLogging

    -- Wait for the threads to shut down.
    traverse_ (atomically . takeTMVar) (lm : wm : ms)

    putStrLn "Done"
