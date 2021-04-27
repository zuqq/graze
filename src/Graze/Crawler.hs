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
-- sure not to visit the same URL twice; it creates a 'Node' for every visited
-- URL and writes it to the 'output' queue that was passed in.
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
import Graze.Node
import Graze.URI

data Job = Job
    { jobOrigin :: !URI
    , jobTarget :: !URI
    , jobDepth  :: !Int
    }
    deriving (Eq, Ord, Show)

fetch
    :: IO (Maybe Job)
    -> (Maybe (Job, Set URI) -> IO ())
    -> IO ()
fetch receive send = loop
  where
    loop =
        receive >>= \case
            Nothing -> pure ()
            Just job@Job {..} -> do
                try (get ("text" // "html") "graze" jobTarget) >>= \case
                    Left (_ :: GrazeHttpException) -> send Nothing
                    Right s -> send (Just (job, parseLinks jobTarget s))
                loop

data CrawlerOptions = CrawlerOptions
    { base      :: URI            -- ^ URL to start at.
    , crawlable :: URI -> Bool    -- ^ Selects links to follow.
    , depth     :: Int            -- ^ Depth of the search.
    , threads   :: Int            -- ^ Size of the thread pool.
    , output    :: TBMQueue Node  -- ^ Output queue.
    }

-- | A wrapper around 'URI' for defining custom 'Eq' and 'Ord' instances.
newtype SeenURI = SeenURI URI

-- | Extract authority, path, and query.
extractRelevant :: SeenURI -> (Maybe URIAuth, String, String)
extractRelevant (SeenURI uri) = (uriAuthority uri, uriPath uri, uriQuery uri)

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Eq SeenURI where
    x == y = extractRelevant x == extractRelevant y

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Ord SeenURI where
    x <= y = extractRelevant x <= extractRelevant y

-- | Run the crawler. 
--
-- Returns when there are no more URLs to visit.
crawl :: CrawlerOptions -> IO ()
crawl CrawlerOptions {..} = do
    fetchOutput <- newTBQueueIO (fromIntegral threads)

    fetchInput <- newTMQueueIO
    atomically (writeTMQueue fetchInput (Job base base 0))

    let makeJobs seen Job {..} links
            | jobDepth >= depth = (mempty, seen)
            | otherwise         = (jobs, seen')
          where
            links' =
                Set.difference
                    (Set.map SeenURI (Set.filter crawlable links))
                    seen
            jobs   =
                Set.map
                    (\(SeenURI uri) -> Job jobTarget uri (jobDepth + 1)) links'
            seen'  = Set.union seen links'

    let loop seen open
            | open <= 0 = atomically (closeTMQueue fetchInput)
            | otherwise = do
                report <- atomically (readTBQueue fetchOutput)
                (seen', open') <-
                    case report of
                        Nothing -> pure (seen, open)
                        Just (job@Job {..}, links) -> do
                            atomically
                                (writeTBMQueue output
                                    (Node jobOrigin jobTarget links))
                            let (jobs, seen') = makeJobs seen job links
                            traverse_
                                (atomically . writeTMQueue fetchInput)
                                jobs
                            pure (seen', open + Set.size jobs)
                loop seen' (open' - 1)

    concurrently_
        (replicateConcurrently_
            threads
            (fetch
                (atomically (readTMQueue fetchInput))
                (atomically . writeTBQueue fetchOutput)))
        (loop (Set.singleton (SeenURI base)) 1)

    atomically (closeTBMQueue output)
