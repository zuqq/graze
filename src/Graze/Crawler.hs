{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Crawler
    ( Chans (Chans)
    , Config (Config)
    , run
    ) where

import Control.Concurrent.STM           (atomically)
import Control.Concurrent.STM.TChan     (TChan, readTChan, writeTChan)
import Control.Monad                    (unless)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT, get, gets, modify', put)
import Data.Foldable                    (foldl', traverse_)

import qualified Data.HashSet as HS (HashSet, insert, member, singleton)

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

data Triple a b c = Triple !a !b !c

process
    :: HS.HashSet HttpUrl    -- Seen URLs before.
    -> [HttpUrl]             -- Links.
    -> ( HS.HashSet HttpUrl  -- Seen URLs after.
       , Int                 -- Number of new URLs.
       , [HttpUrl]           -- List of new URLs.
       )
process s xs = done $ foldl' step (Triple s 0 id) xs
  where
    step (Triple s' i ys) x = if x `HS.member` s'
        then Triple s' i ys
        else Triple (x `HS.insert` s') (i + 1) ((x :) . ys)
    done (Triple s' i ys)   = (s', i, ys [])

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    atomically $ writeTChan outbox (Fetch (Job base base depth))
    evalStateT loop (Browser 1 (HS.singleton base))
  where
    loop = do
        (liftIO . atomically $ readTChan inbox) >>= \case
            Failure                     -> return ()
            Success Job {..} links body -> do
                liftIO . atomically $
                    writeTChan writer (Write (Record origin url links) body)
                unless (depth <= 0) $ do
                    Browser n s <- get
                    let (s', i, links') = process s . filter legal $ links
                    liftIO . atomically $
                        traverse_
                            (writeTChan outbox . Fetch)
                            [Job url link (depth - 1) | link <- links']
                    put $! Browser (n + i) s'
        n <- gets open
        modify' $ \s -> s {open = n - 1}
        unless (n <= 1) loop
