{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Crawler (crawl) where

import Control.Concurrent.Async (concurrently_, replicateConcurrently_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue
import Control.Exception (try)
import Data.Foldable (traverse_)
import Data.Set (Set)

import qualified Data.Set as Set

import Graze.HTML
import Graze.Http
import Graze.Record
import Graze.URI

-- | A job for a fetcher.
data Job = Job
    { jobOrigin :: !URI
    , jobTarget :: !URI
    , jobDepth  :: !Int  -- ^ Remaining depth of the search.
    }
    deriving (Eq, Ord, Show)

-- | The report that the fetcher sends back to the crawler.
data JobReport
    = Failure
    | Success
        !Job
        !(Set URI)  -- ^ Outgoing links.
    deriving (Eq, Ord, Show)

-- | Fetcher thread.
--
-- A unit of work for a fetcher consists of downloading and parsing a page;
-- it then passes the result back to the crawler.
fetch
    :: IO (Maybe Job)        -- ^ Receive a 'Job'.
    -> (JobReport -> IO ())  -- ^ Send a 'JobReport' to the crawler.
    -> IO ()
fetch receive send = loop
  where
    loop =
        receive >>= \case
            Nothing -> pure ()
            Just job@Job {..} -> do
                try (get ("text" // "html") "graze" jobTarget) >>= \case
                    Left (_ :: HttpException) -> send Failure
                    Right (Just s) ->
                        send (Success job (parseLinks jobTarget s))
                    Right _ -> send (Success job mempty)
                loop

-- | Crawler thread.
--
-- The crawler creates 'Job's for the fetchers to complete. It keeps track of
-- the set of seen URIs and the number of open jobs. When the number of open
-- jobs hits zero, the crawler thread closes the result queue, shuts down the
-- fetchers, and exits.
crawl
    :: TBMQueue Record  -- ^ Output queue.
    -> URI              -- ^ URI to start at.
    -> (URI -> Bool)    -- ^ Selects URIs to visit.
    -> Int              -- ^ Depth of the search.
    -> Int              -- ^ Number of threads.
    -> IO ()
crawl recordQueue base crawlable depth threads = do
    jobReportQueue <- newTBQueueIO (fromIntegral threads)

    jobQueue <- newTMQueueIO
    atomically (writeTMQueue jobQueue (Job base base depth))

    let makeJobs seen Failure                  = (mempty, seen)
        makeJobs seen (Success Job {..} links) =
            if jobDepth <= 0
                then (mempty, seen)
                else (jobs, seen')
          where
            links' = Set.difference (Set.filter crawlable links) seen
            jobs   = Set.map (\link -> Job jobTarget link (jobDepth - 1)) links'
            seen'  = Set.union seen links'

    let loop seen open = do
            report <- atomically (readTBQueue jobReportQueue)
            case report of
                Failure -> mempty
                Success Job {..} links ->
                      atomically
                    . writeTBMQueue recordQueue
                    $ Record jobOrigin jobTarget links
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
                (atomically . writeTBQueue jobReportQueue)))
        (loop (Set.singleton base) 1)

    atomically (closeTBMQueue recordQueue)
