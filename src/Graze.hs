{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module ties together the different components of the system.
--
-- Concurrency is achieved through the creation of multiple threads that
-- use closeable queues to communicate. The main thread spawns
--
--     * a "Graze.Crawler" thread,
--
--     * a "Graze.Logger" thread,
--
--     * a "Graze.Writer" thread,
--
--     * and 'threads' "Graze.Fetcher" threads.

module Graze
    ( Config (..)
    , parseUrl
    , run
    ) where

import           Control.Concurrent.Async       (Concurrently (..))
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Q
import qualified Control.Concurrent.STM.TMQueue as Q
import qualified Control.Concurrent.STM.TBMQueue as Q
import           Control.Exception              (try)
import           Control.Monad                  (replicateM_)
import qualified Data.ByteString.Lazy           as BL (toStrict)
import qualified Data.Text                      as T (unpack)
import qualified Data.Text.Encoding             as T (decodeUtf8')

import qualified Data.HashSet as HS (singleton)
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

    fetcherQueue <- Q.newTMQueueIO
    {-
        These queues are bounded in order to prevent the threads that process
        the results from being overwhelmed. If any of them is full, all fetcher
        threads block; this gives the other threads a chance to catch up. On
        the other hand, 'fetcherQueue' needs to be unbounded because consuming
        a value from 'crawlerQueue' causes writes to 'fetcherQueue'; otherwise
        this cyclic dependency could cause a deadlock.
    -}
    writerQueue <- Q.newTBMQueueIO threads
    loggerQueue <- Q.newTBMQueueIO threads
    crawlerQueue <- Q.newTBQueueIO (fromIntegral threads)

    let runFetcher' = runFetcher
            (Q.readTMQueue fetcherQueue)
            (Q.writeTBMQueue writerQueue)
            (Q.writeTBMQueue loggerQueue)
            (Q.writeTBQueue crawlerQueue)

    let runWriter' = runWriter folder (Q.readTBMQueue writerQueue)

    let runLogger' = runLogger (Q.readTBMQueue loggerQueue)

    response :: Either H.HttpException Response <- try $
        get base {path = "/robots.txt"}
    let robots  = case response of
            Right (TextPlain, bs) -> case T.decodeUtf8' . BL.toStrict $ bs of
                Left _  -> const True
                Right s -> parseRobots "graze" s
            _                     -> const True

    let runCrawler' = do
            atomically . Q.writeTMQueue fetcherQueue $ Job base base depth
            runCrawler
                (CrawlerState (HS.singleton base) 1)
                (\x -> domain x == domain base && robots (path x))
                (Q.readTBQueue crawlerQueue)
                (Q.writeTMQueue fetcherQueue)
            atomically $ Q.closeTMQueue fetcherQueue
            atomically $ Q.closeTBMQueue writerQueue
            atomically $ Q.closeTBMQueue loggerQueue

    {-
        Build a 'Concurrently' value that pools the individual threads.

        The crucial advantage over plain forking is that an uncaught exception
        in any of the threads causes everything to be shut down.
    -}
    runConcurrently $
        replicateM_ threads (Concurrently runFetcher')
            *> Concurrently runWriter'
            *> Concurrently runLogger'
            *> Concurrently runCrawler'

    putStrLn "Done"
