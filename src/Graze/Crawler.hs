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

import Graze.HttpUrl (HttpUrl)
import Graze.Types


-- process ---------------------------------------------------------------------

-- Auxiliary triple type that is strict in every argument.
data Triple a b c = Triple !a !b !c

-- Process a list of links in a single pass. The third component of the
-- accumulator is a difference list; it is converted to an ordinary list in the
-- last step.
process
    :: HS.HashSet HttpUrl    -- Seen URLs before.
    -> [HttpUrl]             -- Links.
    -> ( HS.HashSet HttpUrl  -- Seen URLs after.
       , Int                 -- Number of new URLs.
       , [HttpUrl]           -- List of new URLs.
       )
process s xs = done $ foldl' step (Triple s 0 id) xs
  where
    step (Triple s' i ys) x = if x `HS.member` s'
        then Triple s' i ys
        else Triple (x `HS.insert` s') (i + 1) (ys . (x :))
    done (Triple s' i ys)   = (s', i, ys [])

-- CrawlerConfig ---------------------------------------------------------------

data CrawlerConfig = CrawlerConfig
    { base  :: !HttpUrl            -- ^ Base URL.
    , depth :: !Int                -- ^ Depth of the search.
    , legal :: !(HttpUrl -> Bool)  -- ^ Predicate that selects URLs to crawl.
    }

-- CrawlerState ----------------------------------------------------------------

-- Apart from the set of seen URLs, the main thread also maintains a counter for
-- the number of open (i.e., uncompleted) jobs. If this counter reaches zero,
-- the program exits.
data CrawlerState = CrawlerState
    !(HS.HashSet HttpUrl)  -- ^ Seen URLs.
    !Int                   -- ^ Number of open jobs.

seen :: Lens' CrawlerState (HS.HashSet HttpUrl)
seen p (CrawlerState s i) = fmap (`CrawlerState` i) (p s)

open :: Lens' CrawlerState Int
open q (CrawlerState s i) = fmap (s `CrawlerState`) (q i)

-- Crawler ---------------------------------------------------------------------

type Crawler a = StateT CrawlerState IO a

evalCrawler :: Crawler a -> CrawlerState -> IO a
evalCrawler = evalStateT

crawler :: (HttpUrl -> Bool) -> Queues -> Crawler ()
crawler legal Queues {..} = loop
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

runCrawler :: CrawlerConfig -> Queues -> IO ()
runCrawler CrawlerConfig {..} queues = do
    atomically . writeTQueue (fetcherQueue queues) $ Fetch (Job base base depth)
    evalCrawler (crawler legal queues) (CrawlerState (HS.singleton base) 1)
