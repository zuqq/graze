{-# LANGUAGE RecordWildCards   #-}

module Graze.Crawler
    ( CrawlerState (..)
    , crawl
    , evalCrawler
    ) where

import Control.Concurrent.STM           (atomically)
import Control.Concurrent.STM.TChan     (TChan, readTChan, writeTChan)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.State.Strict
    ( StateT
    , evalStateT
    , get
    , gets
    , modify
    , put
    )
import Data.Foldable                    (traverse_)
import qualified Data.ByteString    as B (ByteString)
import qualified Data.Set           as S (Set, insert, member)


import Graze.HttpUrl  (HttpUrl)
import Graze.Links    (links)
import Graze.Messages


data CrawlerState = CrawlerState
    { csActive :: !Int
    , csSeen   :: !(S.Set HttpUrl)
    }

type Crawler a = StateT CrawlerState IO a

evalCrawler :: Crawler a -> CrawlerState -> IO a
evalCrawler = evalStateT

crawl
    :: (HttpUrl -> Bool)
    -> TChan Job
    -> TChan Report
    -> TChan Instruction
    -> Crawler ()
crawl p jobChan repChan outChan = loop
  where
    loop = do
        Report Job {..} res <- liftIO . atomically $ readTChan repChan
        modify $ \s -> s {csActive = csActive s - 1}
        case res of
            Fail         -> return ()
            Success cont -> do
                let urls = links jUrl cont
                liftIO . atomically $
                    writeTChan outChan $
                        Write (Record jParent jUrl urls cont)
                if jDepth <= 0
                    then return ()
                    else do
                        CrawlerState {..} <- get
                        let p' url = p url && not (url `S.member` csSeen)
                            urls'  = filter p' urls
                            jobs   = Job (jDepth - 1) jUrl <$> urls'
                        liftIO . atomically $
                            traverse_ (writeTChan jobChan) jobs
                        put $ CrawlerState
                            (csActive + length urls')
                            (foldr S.insert csSeen urls')
        n <- gets csActive
        if n <= 0
            then liftIO . atomically $ writeTChan outChan Stop
            else loop
