{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Crawler
    ( CrawlerConfig (..)
    , runCrawler
    ) where

import Control.Concurrent.STM           (atomically)
import Control.Concurrent.STM.TQueue    (writeTQueue)
import Control.Concurrent.STM.TBQueue   (readTBQueue)
import Control.Monad                    (unless)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Foldable                    (foldl', traverse_)

import qualified Data.HashSet   as HS (HashSet, insert, member, singleton)
import           Lens.Micro     (Lens')
import           Lens.Micro.Mtl ((+=), (-=), (.=), use)

import Graze.Url   (Url)
import Graze.Types (FetcherCommand (..), Job (..), Queues (..), Result (..))


-- CrawlerState ----------------------------------------------------------------

-- | Apart from the set of seen URLs, the crawler also maintains a counter for
-- the number of open jobs. When this counter reaches zero, the crawler exits.
data CrawlerState = CrawlerState
    !(HS.HashSet Url)  -- ^ Set of seen URLs.
    !Int               -- ^ Number of open jobs.

-- | Set of seen URLs.
seen :: Lens' CrawlerState (HS.HashSet Url)
seen p (CrawlerState s i) = fmap (`CrawlerState` i) (p s)

-- | Number of open jobs.
open :: Lens' CrawlerState Int
open q (CrawlerState s i) = fmap (s `CrawlerState`) (q i)

-- process ---------------------------------------------------------------------

-- | An auxiliary triple type that is strict in every argument.
data Triple a b c = Triple !a !b !c

-- | Process a list of links in a single pass.
--
-- Note that this needs to be a left fold: the tail of the list can only be
-- processed after the head is added to the set of seen URLs, because we want to
-- avoid duplicates.
--
-- The third component of the accumulator is a difference list containing the
-- new links; it is converted to an ordinary list by @done@.
process
    :: HS.HashSet Url    -- Set of seen URLs before.
    -> [Url]             -- List of URLs to process.
    -> ( HS.HashSet Url  -- Set of seen URLs after.
       , Int             -- Number of new URLs.
       , [Url]           -- List of new URLs.
       )
process s xs = done $ foldl' step (Triple s 0 id) xs
  where
    step (Triple s' i ys) x = if x `HS.member` s'
        then Triple s' i ys
        else Triple (x `HS.insert` s') (i + 1) (ys . (x :))
    done (Triple s' i ys)   = (s', i, ys [])

-- Crawler ---------------------------------------------------------------------

type Crawler a = StateT CrawlerState IO a

evalCrawler :: Crawler a -> CrawlerState -> IO a
evalCrawler = evalStateT

crawl :: (Url -> Bool) -> Queues -> Crawler ()
crawl legal Queues {..} = loop
  where
    loop = do
        (liftIO . atomically . readTBQueue $ resultQueue) >>= \case
            Failure                -> return ()
            Success Job {..} links -> unless (depth <= 0) $ do
                s <- use seen
                let (s', i, links') = process s . filter legal $ links
                traverse_ (liftIO . atomically . writeTQueue fetcherQueue)
                    [Fetch (Job url link (depth - 1)) | link <- links']
                seen .= s'
                open += i
        open -= 1
        n <- use open
        unless (n <= 0) loop

-- CrawlerConfig ---------------------------------------------------------------

data CrawlerConfig = CrawlerConfig
    { base  :: Url          -- ^ Base URL.
    , depth :: Int          -- ^ Depth of the search.
    , legal :: Url -> Bool  -- ^ Predicate that selects URLs to crawl.
    }

runCrawler :: CrawlerConfig -> Queues -> IO ()
runCrawler CrawlerConfig {..} queues = do
    atomically . writeTQueue (fetcherQueue queues) $ Fetch (Job base base depth)
    evalCrawler (crawl legal queues) (CrawlerState (HS.singleton base) 1)
