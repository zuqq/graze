{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

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
    { jobParent   :: !URI
    , jobLocation :: !URI
    , jobDepth    :: !Int
    }
    deriving (Eq, Ord, Show)

makeNode :: Job -> Set URI -> Node
makeNode Job {..} links = Node jobParent jobLocation links

makeChildJob :: Job -> URI -> Job
makeChildJob Job {..} uri = Job jobLocation uri (jobDepth + 1)

data Report = Failure | Success !Job !(Set URI)

fetch :: IO (Maybe Job) -> (Report -> IO ()) -> IO ()
fetch receive send = loop
  where
    loop =
            receive
        >>= \case
                Nothing -> pure ()
                Just job@Job {..} ->
                        try (   getOnly ("text" // "html") jobLocation
                            >>= decodeResponse
                            )
                    >>= \case
                            Left (_ :: GrazeHttpException) -> send Failure
                            Right s ->
                                send (Success job (parseLinks jobLocation s))
                    >>  loop

data CrawlerOptions = CrawlerOptions
    { base      :: URI            -- ^ URL to start at.
    , crawlable :: URI -> Bool    -- ^ Selects links to follow.
    , depth     :: Int            -- ^ Depth of the search.
    , threads   :: Int            -- ^ Size of the thread pool.
    , output    :: TBMQueue Node  -- ^ Output queue.
    }

-- | A wrapper around 'URI' for defining custom 'Eq' and 'Ord' instances.
newtype SeenURI = SeenURI {getSeenURI :: URI}

-- | Extract authority, path, and query.
extractRelevant :: SeenURI -> (Maybe URIAuth, String, String)
extractRelevant (SeenURI URI {..}) = (uriAuthority, uriPath, uriQuery)

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Eq SeenURI where
    x == y = extractRelevant x == extractRelevant y

-- | Ignores 'uriScheme' and 'uriFragment'.
instance Ord SeenURI where
    x <= y = extractRelevant x <= extractRelevant y

difference :: Set URI -> Set SeenURI -> (Set URI, Set SeenURI)
difference (Set.map SeenURI -> links) seen = (links', seen')
  where
    links' = Set.map getSeenURI (links `Set.difference` seen)
    seen'  = links `Set.union` seen

-- | Run the crawler. 
--
-- Returns when there are no more URLs to visit.
crawl :: CrawlerOptions -> IO ()
crawl CrawlerOptions {..} = do
    fetchOutput <- newTBQueueIO (fromIntegral threads)

    fetchInput <- newTMQueueIO

    let sendJob = atomically . writeTMQueue fetchInput

    let loop seen open
            -- We need to close @fetchInput@ inside of @loop@ because we are
            -- joining on all threads below.
            | open <= 0 = atomically (closeTMQueue fetchInput)
            | otherwise =
                    atomically (readTBQueue fetchOutput)
                >>= \case
                        Failure -> loop seen (open - 1)
                        Success job@Job {..} links -> do
                            atomically
                                (writeTBMQueue output (makeNode job links))
                            if jobDepth >= depth then
                                loop seen (open - 1)
                            else do
                                let (links', seen') =
                                        Set.filter crawlable links
                                            `difference` seen
                                traverse_ (sendJob . makeChildJob job) links'
                                loop seen' (open - 1 + Set.size links')

    sendJob (Job base base 0)

    concurrently_
        (replicateConcurrently_
            threads
            (fetch
                (atomically (readTMQueue fetchInput))
                (atomically . writeTBQueue fetchOutput)))
        (loop (Set.singleton (SeenURI base)) 1)

    atomically (closeTBMQueue output)
