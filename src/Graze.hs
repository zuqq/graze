{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module ties together the different components of the system.
--
-- Concurrency is achieved through the creation of multiple threads that
-- use queues to communicate. The main thread spawns
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
import           Control.Concurrent.STM.TQueue  (newTQueueIO)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO)
import           Control.Exception              (try)
import           Control.Monad                  (replicateM_)
import qualified Data.ByteString.Lazy           as BL (toStrict)
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

    let n = fromIntegral threads
    queues <- Queues
        <$> newTQueueIO
        <*> newTBQueueIO n
        <*> newTBQueueIO n
        <*> newTBQueueIO n

    response :: Either H.HttpException Response <- try $
        get base {path = "/robots.txt"}
    let robots  = case response of
            Right (TextPlain, bs) -> case T.decodeUtf8' . BL.toStrict $ bs of
                Left _  -> const True
                Right s -> parseRobots "graze" s
            _                     -> const True
        legal x = domain x == domain base && robots (path x)

    {-
        Build a 'Concurrently' value that pools the individual threads.

        The crucial advantage over plain forking is that an uncaught exception
        in any of the threads causes everything to be shut down.
    -}
    runConcurrently $
        replicateM_ threads (Concurrently $ runFetcher queues)
            *> Concurrently (runWriter folder queues)
            *> Concurrently (runLogger queues)
            *> Concurrently (runCrawler CrawlerConfig {..} queues)

    putStrLn "Done"
