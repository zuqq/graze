{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

-- | A web crawler, using a pool of lightweight threads for concurrent crawling.
--
-- The crawler follows links in breadth-first order, starting with 'base'. The
-- 'crawlable' predicate constrains the links that are followed (e.g., only
-- those that stay on the same host and respect the robots.txt file). The
-- maximal depth of the traversal is specified by 'depth'. The crawler makes
-- sure not to visit the same URL twice; it creates a 'Node' for every visited
-- URL and writes it to the 'output' queue that was passed in.
module Graze.Crawler (CrawlerOptions (..), crawl) where

import Control.Concurrent.Async (replicateConcurrently_, withAsync)
import Control.Concurrent.STM
import Control.Exception (try)
import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Set (Set)

import qualified Data.Set as Set

import Graze.HTML
import Graze.Http
import Graze.Node
import Graze.URI

data Job = Job
    { jobDepth    :: !Int
    , jobParent   :: !URI
    , jobLocation :: !URI
    }
    deriving (Eq, Ord, Show)

makeNode :: Job -> Set URI -> Node
makeNode Job {..} = Node jobParent jobLocation

makeChildJob :: Job -> URI -> Job
makeChildJob Job {..} = Job (jobDepth + 1) jobLocation

data Report = Failure | Success !Job !(Set URI)

fetch :: IO Job -> (Report -> IO ()) -> IO ()
fetch receiveJob sendReport = forever (do
    job@Job {..} <- receiveJob
    result <- try @GrazeHttpException (getText jobLocation)
    case result of
        Left _ -> sendReport Failure
        Right s -> sendReport (Success job (parseLinks jobLocation s)))

data CrawlerOptions = CrawlerOptions
    { base      :: URI            -- ^ URL to start at.
    , crawlable :: URI -> Bool    -- ^ Selects links to follow.
    , depth     :: Int            -- ^ Depth of the search.
    , threads   :: Int            -- ^ Size of the thread pool.
    , output    :: Node -> IO ()  -- ^ Output a result.
    }

-- | A wrapper around 'URI' for defining custom 'Eq' and 'Ord' instances.
newtype SeenURI = SeenURI URI

-- | Extract authority, path, and query.
extractSeenURI :: SeenURI -> (Maybe URIAuth, String, String)
extractSeenURI (SeenURI URI {..}) = (uriAuthority, uriPath, uriQuery)

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Eq SeenURI where
    x == y = extractSeenURI x == extractSeenURI y

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Ord SeenURI where
    x <= y = extractSeenURI x <= extractSeenURI y

newURIs :: Set URI -> Set SeenURI -> (Set URI, Set SeenURI)
newURIs (Set.map SeenURI -> links) seen = (links', seen')
  where
    links' = Set.map (\(SeenURI uri) -> uri) (links `Set.difference` seen)
    seen'  = links `Set.union` seen

-- | Run the crawler. 
--
-- Returns when there are no more URLs to visit.
crawl :: CrawlerOptions -> IO ()
crawl CrawlerOptions {..} = do
    fetchInput <- newTQueueIO

    fetchOutput <- newTBQueueIO (fromIntegral threads)

    let sendJob = atomically . writeTQueue fetchInput

    let loop seen open
            | open <= 0 = pure ()
            | otherwise =
                    atomically (readTBQueue fetchOutput)
                >>= \case
                        Failure -> loop seen (open - 1)
                        Success job@Job {..} links -> do
                            output (makeNode job links)
                            if jobDepth >= depth then
                                loop seen (open - 1)
                            else do
                                let (links', seen') =
                                        newURIs
                                            (Set.filter crawlable links)
                                            seen
                                traverse_ (sendJob . makeChildJob job) links'
                                loop seen' (open - 1 + Set.size links')

    let receiveJob = atomically (readTQueue fetchInput)

    let sendReport = atomically . writeTBQueue fetchOutput

    let fetchPool = replicateConcurrently_ threads (fetch receiveJob sendReport)

    withAsync fetchPool (\_ -> do
        sendJob (Job 0 base base)
        loop (Set.singleton (SeenURI base)) 1)
