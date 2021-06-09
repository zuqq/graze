{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

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
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Foldable (traverse_)
import Data.Set (Set)
import Lens.Micro (Lens')
import Lens.Micro.Mtl ((+=), (-=), (.=), use)

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
newtype SeenURI = SeenURI {getURI :: URI}

-- | Extract authority, path, and query.
extractSeenURI :: SeenURI -> (Maybe URIAuth, String, String)
extractSeenURI (SeenURI URI {..}) = (uriAuthority, uriPath, uriQuery)

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Eq SeenURI where
    x == y = extractSeenURI x == extractSeenURI y

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Ord SeenURI where
    x <= y = extractSeenURI x <= extractSeenURI y

data CrawlerState = CrawlerState !(Set SeenURI) !Int

-- | Set of seen URLs.
seen :: Lens' CrawlerState (Set SeenURI)
seen p (CrawlerState s i) = fmap (`CrawlerState` i) (p s)

-- | Number of open jobs.
open :: Lens' CrawlerState Int
open q (CrawlerState s i) = fmap (s `CrawlerState`) (q i)

-- | Run the crawler. 
--
-- Returns when there are no more URLs to visit.
crawl :: CrawlerOptions -> IO ()
crawl CrawlerOptions {..} = do
    fetchInput <- newTQueueIO

    fetchOutput <- newTBQueueIO (fromIntegral threads)

    let sendJob = atomically . writeTQueue fetchInput

    let loop = do
            report <- liftIO (atomically (readTBQueue fetchOutput))
            open -= 1
            case report of
                Failure -> pure ()
                Success job@Job {..} links -> do
                    liftIO (output (makeNode job links))
                    when (jobDepth < depth) (do
                        let links' =
                                Set.map SeenURI (Set.filter crawlable links)
                        s <- use seen
                        seen .= s `Set.union` links'
                        let newURIs = Set.map getURI (links' `Set.difference` s)
                        liftIO (traverse_ (sendJob . makeChildJob job) newURIs)
                        open += Set.size newURIs)
            n <- use open
            when (n > 0) loop

    let receiveJob = atomically (readTQueue fetchInput)

    let sendReport = atomically . writeTBQueue fetchOutput

    let fetchPool = replicateConcurrently_ threads (fetch receiveJob sendReport)

    withAsync fetchPool (\_ -> do
        sendJob (Job 0 base base)
        evalStateT loop (CrawlerState (Set.singleton (SeenURI base)) 1))
