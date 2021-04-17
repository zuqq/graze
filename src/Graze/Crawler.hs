{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Crawler (crawl) where

import Control.Concurrent.Async (concurrently_, replicateConcurrently_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Foldable (foldl', traverse_)
import Data.Set (Set)
import Lens.Micro (Lens')
import Lens.Micro.Mtl ((+=), (-=), (.=), use)

import qualified Data.Set as Set

import Graze.Types
import Graze.URI
import Graze.Fetcher

data CrawlerState = CrawlerState
    !(Set URI)  -- ^ Set of seen URLs.
    !Int            -- ^ Number of open jobs.

-- | Set of seen URLs.
seen :: Lens' CrawlerState (Set URI)
seen p (CrawlerState s i) = fmap (`CrawlerState` i) (p s)

-- | Number of open jobs.
open :: Lens' CrawlerState Int
open q (CrawlerState s i) = fmap (s `CrawlerState`) (q i)

-- | Process a list of links in a single pass.
process
    :: Set URI    -- Set of seen URLs before.
    -> [URI]          -- List of URLs to process.
    -> ( Set URI  -- Set of seen URLs after.
       , Int          -- Number of new URLs.
       , [URI]        -- List of new URLs.
       )
process s xs = done $ foldl' step (s, 0, id) xs
  where
    -- The third component of the accumulator is a difference list containing
    -- the new links; it is converted to an ordinary list by @done@.
    step (!s', !i, !ys) x = if x `Set.member` s'
        then (s', i, ys)
        else (x `Set.insert` s', i + 1, ys . (x :))
    done (!s', !i, !ys)   = (s', i, ys [])

-- | Crawler thread.
--
-- The crawler thread creates 'Job's for the fetcher threads to complete. To do
-- so, it maintains the set of seen URLs and a counter for the number of open
-- jobs.
--
-- When the number of open jobs hits zero, the crawler thread closes the result
-- queue, shuts down the fetchers, and exits.
crawl
    :: TBMQueue Result  -- ^ Result queue.
    -> URI              -- ^ URL to start at.
    -> (URI -> Bool)    -- ^ Determines URLs to visit.
    -> Int              -- ^ Depth of the search.
    -> Int              -- ^ Number of threads.
    -> IO ()
crawl results base legal depth_ threads = do
    reports <- newTBQueueIO (fromIntegral threads)

    jobs <- newTMQueueIO
    atomically (writeTMQueue jobs (Job base base depth_))

    let loop = do
            report <- liftIO . atomically . readTBQueue $ reports
            case report of
                Failure                   -> pure ()
                Success Job {..} links bs -> do
                    unless (depth <= 0) $ do
                        s <- use seen
                        let (s', i, links') = process s . filter legal $ links
                        traverse_
                            (liftIO . atomically . writeTMQueue jobs)
                            [Job url link (depth - 1) | link <- links']
                        seen .= s'
                        open += i
                    liftIO . atomically . writeTBMQueue results $
                        Result (Record origin url links) bs
            open -= 1
            n <- use open
            if n <= 0
                then liftIO . atomically . closeTMQueue $ jobs
                else loop

    concurrently_
        (replicateConcurrently_
            threads
            (fetch (readTMQueue jobs) (writeTBQueue reports)))
        (evalStateT loop (CrawlerState (Set.singleton base) 1))

    atomically (closeTBMQueue results)
