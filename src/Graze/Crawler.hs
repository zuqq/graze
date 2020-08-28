{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Crawler
    ( CrawlerConfig (..)
    , runCrawler
    ) where

import Control.Concurrent.STM           (atomically)
import Control.Concurrent.STM.TChan     (TChan, readTChan, writeTChan)
import Control.Monad                    (unless)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify', put)
import Data.Foldable                    (foldl', traverse_)

import qualified Data.HashSet as HS (HashSet, insert, member, singleton)

import Graze.HttpUrl  (HttpUrl)
import Graze.Messages
import Graze.Util     (readFrom, writeTo, writeManyTo)


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

data CrawlerConfig = CrawlerConfig
    { base  :: !HttpUrl            -- ^ Base URL.
    , depth :: !Int                -- ^ Depth of the search.
    , legal :: !(HttpUrl -> Bool)  -- ^ Predicate for selecting URLs to crawl.
    }

data CrawlerState = CrawlerState
    { seen :: !(HS.HashSet HttpUrl)
    , open :: !Int
    }

type Crawler a = StateT CrawlerState IO a

evalCrawler :: Crawler a -> CrawlerState -> IO a
evalCrawler = evalStateT

runCrawler :: CrawlerConfig -> Chans -> IO ()
runCrawler CrawlerConfig {..} Chans {..} = do
    writeTo fetcherChan $ Fetch (Job base base depth)
    evalCrawler loop (CrawlerState (HS.singleton base) 1)
  where
    loop = do
        readFrom resultChan >>= \case
            Failure                     -> return ()
            Success Job {..} links body -> do
                writeTo writerChan $ Write (Record origin url links) body
                unless (depth <= 0) $ do
                    CrawlerState s n <- get
                    let (s', i, links') = process s . filter legal $ links
                    writeManyTo fetcherChan
                        [Fetch (Job url link (depth - 1)) | link <- links']
                    put $! CrawlerState s' (n + i)
        n <- gets open
        modify' $ \s -> s {open = n - 1}
        unless (n <= 1) loop
