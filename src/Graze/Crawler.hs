{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Crawler
    ( CrawlerConfig (..)
    , runCrawler
    ) where

import Control.Concurrent.STM           (atomically)
import Control.Concurrent.STM.TChan     (TChan, readTChan, writeTChan)
import Control.Monad                    (unless)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Foldable                    (foldl', traverse_)

import qualified Data.HashSet   as HS (HashSet, insert, member, singleton)
import           Lens.Micro     (Lens')
import           Lens.Micro.Mtl ((+=), (-=), (.=), use)

import Graze.HttpUrl  (HttpUrl)
import Graze.Messages


-- process ---------------------------------------------------------------------

data Triple a b c = Triple !a !b !c

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
    , legal :: !(HttpUrl -> Bool)  -- ^ Predicate for selecting URLs to crawl.
    }

-- CrawlerState ----------------------------------------------------------------

data CrawlerState = CrawlerState
    { _seen :: !(HS.HashSet HttpUrl)
    , _open :: !Int
    }

seen :: Lens' CrawlerState (HS.HashSet HttpUrl)
seen p (CrawlerState s i) = fmap (`CrawlerState` i) (p s)

open :: Lens' CrawlerState Int
open q (CrawlerState s i) = fmap (s `CrawlerState`) (q i)

-- Crawler ---------------------------------------------------------------------

type Crawler a = StateT CrawlerState IO a

evalCrawler :: Crawler a -> CrawlerState -> IO a
evalCrawler = evalStateT

writeTo :: MonadIO m => TChan a -> a -> m ()
writeTo chan = liftIO . atomically . writeTChan chan

runCrawler :: CrawlerConfig -> Chans -> IO ()
runCrawler CrawlerConfig {..} chans = do
    writeTo (fetcherChan chans) $ Fetch (Job base base depth)
    evalCrawler (defaultCrawler legal chans) (CrawlerState (HS.singleton base) 1)

defaultCrawler :: (HttpUrl -> Bool) -> Chans -> Crawler ()
defaultCrawler legal Chans {..} = loop
  where
    loop = do
        (liftIO . atomically . readTChan $ resultChan) >>= \case
            Failure                     -> return ()
            Success Job {..} links body -> do
                writeTo writerChan $ Write (Record origin url links) body
                unless (depth <= 0) $ do
                    s <- use seen
                    let (s', i, links') = process s . filter legal $ links
                    liftIO . atomically . traverse_ (writeTChan fetcherChan) $
                        [Fetch (Job url link (depth - 1)) | link <- links']
                    seen .= s'
                    open += i
        open -= 1
        n <- use open
        unless (n <= 0) loop
