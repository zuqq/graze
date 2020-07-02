{-# LANGUAGE RecordWildCards   #-}

module Graze.Crawler
    ( Chans (Chans)
    , Config (Config)
    , run
    ) where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TChan     (TChan, readTChan, writeTChan)
import           Control.Monad                    (unless)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                    (traverse_)
import qualified Data.HashSet                     as HS

import Graze.HttpUrl  (HttpUrl)
import Graze.Messages


data Config = Config
    { depth :: !Int
    , base  :: !HttpUrl
    , legal :: !(HttpUrl -> Bool)
    }

data Chans = Chans
    { inbox  :: !(TChan FetchResult)
    , outbox :: !(TChan FetchCommand)
    , writer :: !(TChan WriteCommand)
    }

data Browser = Browser
    { open :: !Int
    , seen :: !(HS.HashSet HttpUrl)
    }

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    atomically $
        writeTChan outbox $
            Fetch (Job depth base base)
    evalStateT loop (Browser 1 (HS.singleton base))
  where
    loop = do
        result <- liftIO . atomically $ readTChan inbox
        modify' $ \s -> s {open = open s - 1}
        case result of
            Failure        -> return ()
            Success record -> do
                liftIO . atomically $
                    writeTChan writer (Write record)
                let Job {..} = rJob record
                unless (jDepth <= 0) $ do
                    Browser {..} <- get
                    let legal' url = legal url && not (url `HS.member` seen)
                        urls = filter legal' (rLinks record)
                        jobs = Job (jDepth - 1) jUrl <$> urls
                    liftIO . atomically $
                        traverse_ (writeTChan outbox . Fetch) jobs
                    put $ Browser
                        (open + length urls)
                        (foldr HS.insert seen urls)
        n <- gets open
        unless (n <= 0) loop
