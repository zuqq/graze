{-# LANGUAGE RecordWildCards #-}

module Graze.Crawler (crawl) where

import Control.Concurrent.Async (concurrently_, replicateConcurrently_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue
import Data.Foldable (traverse_)
import Data.Set (Set)

import qualified Data.Set as Set

import Graze.Types
import Graze.URI
import Graze.Fetcher

-- | Crawler thread.
--
-- The crawler thread creates 'Job's for the fetcher threads to complete. To do
-- so, it maintains the set of seen URLs and a counter for the number of open
-- jobs.
--
-- When the number of open jobs hits zero, the crawler thread closes the result
-- queue, shuts down the fetchers, and exits.
crawl
    :: TBMQueue Record  -- ^ Record queue.
    -> URI              -- ^ URL to start at.
    -> (URI -> Bool)    -- ^ Determines URLs to visit.
    -> Int              -- ^ Depth of the search.
    -> Int              -- ^ Number of threads.
    -> IO ()
crawl recordQueue base robots depth_ threads = do
    reportQueue <- newTBQueueIO (fromIntegral threads)

    jobQueue <- newTMQueueIO
    atomically (writeTMQueue jobQueue (Job base base depth_))

    let makeJobs :: Set URI -> Report -> (Set Job, Set URI)
        makeJobs seen Failure                  = (mempty, seen)
        makeJobs seen (Success Job {..} links) =
            if depth <= 0
                then (mempty, seen)
                else (jobs, seen')
          where
            links' = Set.difference (Set.filter robots links) seen
            jobs   = Set.map (\link -> Job uri link (depth - 1)) links'
            seen'  = Set.union seen links'

    let loop seen open = do
            report <- atomically (readTBQueue reportQueue)
            case report of
                Failure -> mempty
                Success Job {..} links ->
                      atomically
                    . writeTBMQueue recordQueue
                    $ Record origin uri links
            let (jobs, seen') = makeJobs seen report
            traverse_ (atomically . writeTMQueue jobQueue) jobs
            let open' = open - 1 + Set.size jobs
            if open' <= 0
                then atomically (closeTMQueue jobQueue)
                else loop seen' open'

    concurrently_
        (replicateConcurrently_
            threads
            (fetch
                (atomically (readTMQueue jobQueue))
                (atomically . writeTBQueue reportQueue)))
        (loop (Set.singleton base) 1)

    atomically (closeTBMQueue recordQueue)
