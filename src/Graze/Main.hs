{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Main
    ( Config (..)
    , runMain
    ) where

import           Control.Concurrent           (forkFinally)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import           Control.Concurrent.STM.TMVar
import           Control.Monad                (replicateM, replicateM_)
import           Data.Foldable                (traverse_)
import qualified Data.Text                    as T (unpack)

import Network.HTTP.Client.TLS (newTlsManager, setGlobalManager)

import Graze.Http     (getRobots)
import Graze.HttpUrl  (HttpUrl (..), serializeUrl)
import Graze.Messages

import Graze.Crawler
import Graze.Fetcher
import Graze.Logger
import Graze.Writer


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

    fetcherChan <- newTChanIO
    resultChan  <- newTChanIO
    writerChan  <- newTChanIO
    loggerChan  <- newTChanIO

    let chans = Chans {..}

    let forkChild x = do
            m <- atomically newEmptyTMVar
            _ <- forkFinally x (\_ -> atomically $ putTMVar m ())
            return m

    lm <- forkChild $ runLogger chans

    wm <- forkChild $ runWriter folder chans

    ms <- replicateM threads . forkChild $ runFetcher chans

    p <- getRobots base
    let legal url = domain url == domain base && p (path url)
    runCrawler CrawlerConfig {..} chans

    atomically $ do
        replicateM_ threads $ writeTChan fetcherChan StopFetching
        writeTChan writerChan StopWriting
        writeTChan loggerChan StopLogging

    traverse_ (atomically . takeTMVar) (lm : wm : ms)

    putStrLn "Done"
