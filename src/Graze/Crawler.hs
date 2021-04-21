{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A web crawler, using a pool of lightweight threads for concurrent crawling.
--
-- The crawler follows links in breadth-first order, starting with 'base'. The
-- 'crawlable' predicate constrains the links that are followed (e.g., only
-- those that stay on the same host and respect the robots.txt file). The
-- maximal depth of the traversal is specified by 'depth'. The crawler makes
-- sure not to visit the same URL twice; it creates a 'Record' for every
-- visited URL and writes it to the 'recordQueue' that was passed in.
module Graze.Crawler (CrawlerOptions (..), crawl) where

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

data Job = Job
    { jobOrigin :: !URI
    , jobTarget :: !URI
    , jobDepth  :: !Int  -- ^ Remaining depth of the search.
    }
    deriving (Eq, Ord, Show)

data JobReport
    = Failure
    | Success
        !Job
        !(Set URI)  -- ^ Outgoing links.
    deriving (Eq, Ord, Show)

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

data CrawlerOptions = CrawlerOptions
    { base        :: URI              -- ^ URL to start at.
    , crawlable   :: URI -> Bool      -- ^ Selects links to follow.
    , depth       :: Int              -- ^ Depth of the search.
    , threads     :: Int              -- ^ Size of the thread pool.
    , recordQueue :: TBMQueue Record  -- ^ Output queue.
    }

-- | Run the crawler. 
--
-- Returns when there are no more URLs to visit.
crawl :: CrawlerOptions -> IO ()
crawl CrawlerOptions {..} = do
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
