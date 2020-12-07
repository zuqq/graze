{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Crawler
    ( CrawlerState (..)
    , runCrawler
    )
    where

import Control.Concurrent.STM (STM, atomically)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Foldable (foldl', traverse_)
import qualified Data.HashSet as HS (HashSet, insert, member)
import Lens.Micro (Lens')
import Lens.Micro.Mtl ((+=), (-=), (.=), use)

import Graze.Types
import Graze.Url

data CrawlerState = CrawlerState
    !(HS.HashSet Url)  -- ^ Set of seen URLs.
    !Int               -- ^ Number of open jobs.

-- | Set of seen URLs.
seen :: Lens' CrawlerState (HS.HashSet Url)
seen p (CrawlerState s i) = fmap (`CrawlerState` i) (p s)

-- | Number of open jobs.
open :: Lens' CrawlerState Int
open q (CrawlerState s i) = fmap (s `CrawlerState`) (q i)

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
process s xs = done $ foldl' step (s, 0, id) xs
  where
    step (!s', !i, !ys) x = if x `HS.member` s'
        then (s', i, ys)
        else (x `HS.insert` s', i + 1, ys . (x :))
    done (!s', !i, !ys)   = (s', i, ys [])

-- | Processes 'Result's.
--
-- The crawler thread creates 'Job's for the fetcher threads to complete. To do
-- so, it maintains the set of seen URLs and a counter for the number of open
-- jobs.
--
-- When the number of open jobs hits zero, the crawler thread exists; then the
-- main thread instructs all other threads to shut down by closing their queues.
runCrawler
    :: CrawlerState     -- ^ Initial state.
    -> (Url -> Bool)    -- ^ Determines URLs to visit.
    -> STM Result       -- ^ Receive a 'Result'.
    -> (Job -> STM ())  -- ^ Send a 'Job'.
    -> IO ()
runCrawler initial legal recv sendFetcher = evalStateT loop initial
  where
    loop = do
        (liftIO . atomically $ recv) >>= \case
            Failure                -> pure ()
            Success Job {..} links -> unless (depth <= 0) $ do
                s <- use seen
                let (s', i, links') = process s . filter legal $ links
                traverse_
                    (liftIO . atomically . sendFetcher)
                    [Job url link (depth - 1) | link <- links']
                seen .= s'
                open += i
        open -= 1
        n <- use open
        unless (n <= 0) loop
