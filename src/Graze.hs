{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module ties together the different components of the system.
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

module Graze
    ( Config (..)
    , parseUrl
    , run
    ) where

import           Control.Concurrent.Async       (async, wait)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TQueue  (newTQueueIO, writeTQueue)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import           Control.Exception              (try)
import           Control.Monad                  (replicateM, replicateM_)
import qualified Data.ByteString.Lazy           as BL (toStrict)
import           Data.Foldable                  (traverse_)
import qualified Data.Text                      as T (unpack)
import qualified Data.Text.Encoding             as T (decodeUtf8')

import qualified Network.HTTP.Client     as H (HttpException)
import qualified Network.HTTP.Client.TLS as H (newTlsManager, setGlobalManager)

import Graze.Crawler
import Graze.Fetcher
import Graze.Http
import Graze.Logger
import Graze.Robots
import Graze.Types
import Graze.Url
import Graze.Writer


-- | Configuration for the main thread.
data Config = Config
    { base    :: Url       -- ^ URL to start at.
    , folder  :: FilePath  -- ^ Download folder.
    , depth   :: Int       -- ^ Depth of the search.
    , threads :: Int       -- ^ Number of threads.
    }

-- | Run the main thread.
run :: Config -> IO ()
run Config {..} = do
    putStrLn . T.unpack $ "Crawling " <> serializeUrl base

    tls <- H.newTlsManager
    H.setGlobalManager tls

    fetcherQueue <- newTQueueIO
    -- These queues are bounded in order to provide sufficient backpressure.
    -- If any of them is full, all fetcher threads block. This gives the other
    -- threads a chance to catch up.
    let n = fromIntegral threads
    writerQueue  <- newTBQueueIO n
    loggerQueue  <- newTBQueueIO n
    resultQueue  <- newTBQueueIO n
    let queues = Queues {..}

    fetchers <- replicateM threads . async $ runFetcher queues
    writer   <- async $ runWriter folder queues
    logger   <- async $ runLogger queues

    response :: Either H.HttpException Response <- try $ get base {path = "/robots.txt"}
    let robots  = case response of
            Right (TextPlain, bs) -> case T.decodeUtf8' . BL.toStrict $ bs of
                Left _  -> const True
                Right s -> parseRobots "graze"  s
            _                     -> const True
        legal x = domain x == domain base && robots (path x)
    runCrawler CrawlerConfig {..} queues

    atomically $ do
        replicateM_ threads $ writeTQueue fetcherQueue StopFetching
        writeTBQueue writerQueue StopWriting
        writeTBQueue loggerQueue StopLogging

    traverse_ wait fetchers
    wait writer
    wait logger

    putStrLn "Done"
