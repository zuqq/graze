{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Runners
    ( Config (..)
    , run
    ) where

import Control.Concurrent           (forkFinally)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import Control.Concurrent.STM.TMVar
import Control.Monad                (replicateM, replicateM_)
import Data.Foldable                (traverse_)

import Network.HTTP.Client.TLS (newTlsManager, setGlobalManager)

import Graze.Http     (robots)
import Graze.HttpUrl  (HttpUrl (..), serialize)
import Graze.Messages
import Graze.Robots   (allowedBy)

import qualified Graze.Crawler as Crawler
import qualified Graze.Fetcher as Fetcher
import qualified Graze.Logger  as Logger
import qualified Graze.Writer  as Writer


data Config = Config
    { cWorkers :: !Int       -- ^ Number of worker threads.
    , cDepth   :: !Int       -- ^ Depth of the search.
    , cFolder  :: !FilePath  -- ^ Download folder.
    , cRecords :: !FilePath  -- ^ Page record file.
    , cLog     :: !FilePath  -- ^ Log file.
    , cBase    :: !HttpUrl   -- ^ URL to start at.
    }

run :: Config -> IO ()
run Config {..} = do
    putStrLn $ "Crawling " <> show (serialize cBase)

    tls <- newTlsManager
    setGlobalManager tls

    fetcher <- newTChanIO
    crawler <- newTChanIO
    writer  <- newTChanIO
    logger  <- newTChanIO

    let forkChild x = do
            m <- atomically newEmptyTMVar
            _ <- forkFinally x (\_ -> atomically $ putTMVar m ())
            return m

    lm <- forkChild $ Logger.run
        (Logger.Config cLog)
        (Logger.Chans logger)

    wm <- forkChild $ Writer.run
        (Writer.Config cFolder cRecords)
        (Writer.Chans writer)

    ms <- replicateM cWorkers . forkChild $ Fetcher.run
        (Fetcher.Chans fetcher crawler logger)

    rs <- robots cBase
    let legal url =
            huDomain url == huDomain cBase
            && huPath url `allowedBy` rs
    Crawler.run
        (Crawler.Config cDepth cBase legal)
        (Crawler.Chans crawler fetcher writer)

    atomically $ do
        replicateM_ cWorkers $
            writeTChan fetcher StopFetching
        writeTChan writer StopWriting
        writeTChan logger StopLogging

    traverse_ (atomically . takeTMVar) (lm : wm : ms)
