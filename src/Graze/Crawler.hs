{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Crawler (crawl, initCrawler, evalCrawler) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan, writeTChan)
import Control.Exception (try)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (evalStateT, gets, modify, StateT)
import Data.Foldable (traverse_)
import qualified Data.Set as S (Set, insert, member, singleton)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)

import Network.HTTP.Conduit (HttpException)

import Graze.Http (HttpUrl(..), reqPage)
import Graze.Links (links)
import Graze.Messages
import Graze.Robots (disallowedBy, empty, rules, Rules)


reqRobots :: HttpUrl -> IO Rules
reqRobots url = fmap (rules . TL.decodeUtf8) . reqPage $ robotsUrl
  where
    robotsUrl = url { path = "/robots.txt" }

data CrawlerState = CrawlerState
    { csBase   :: !HttpUrl          -- ^ Page that the crawler started at.
    , csRobots :: !Rules            -- ^ Robots.txt rules for @csBase@'s domain.
    , csSeen   :: !(S.Set HttpUrl)  -- ^ Seen URLs.
    , csActive :: !Int              -- ^ Number of open jobs.
    }

type Crawler a = StateT CrawlerState IO a

evalCrawler :: Crawler a -> CrawlerState -> IO a
evalCrawler = evalStateT

initCrawler :: HttpUrl -> IO CrawlerState
initCrawler base = do
    let s = CrawlerState base empty (S.singleton base) 1
    resp <- try (reqRobots base) :: IO (Either HttpException Rules)
    case resp of
        Left _  -> return s
        Right r -> return s { csRobots = r }

addSeen :: HttpUrl -> Crawler ()
addSeen url = modify $ \s ->
    s { csSeen = S.insert url (csSeen s) }

newUrl :: HttpUrl -> Crawler Bool
newUrl url = do
    base   <- gets csBase
    robots <- gets csRobots
    seen   <- gets csSeen
    if domain url /= domain base
        || url `disallowedBy` robots
        || url `S.member` seen
        then return False
        else addSeen url >> return True

mapActive :: (Int -> Int) -> Crawler ()
mapActive f = modify $ \s ->
    s { csActive = f (csActive s) }

sendJobs
    :: TChan Job
    -> Int        -- ^ Hops
    -> HttpUrl    -- ^ Parent
    -> [HttpUrl]  -- ^ URLs
    -> Crawler ()
sendJobs chan hops parent = liftIO . atomically .
    traverse_ (writeTChan chan . Job hops parent)

crawl
    :: TChan Job
    -> TChan FetchResponse
    -> TChan (Either Done PageRecord)
    -> Crawler ()
crawl jobChan resChan outChan = loop
  where
    loop = do
        response <- liftIO . atomically . readTChan $ resChan
        mapActive (+ (-1))
        let Job {..} = frJob response
        case frResult response of
            Fail         -> return ()
            Success page -> if jHops <= 0
                then return ()
                else do
                    liftIO . atomically $
                        writeTChan outChan . Right $ PageRecord jParent jUrl page
                    ls <- filterM newUrl (links jUrl page)
                    sendJobs jobChan (jHops - 1) jUrl ls
                    mapActive (+ length ls)
        n <- gets csActive
        if n > 0
            then loop
            else liftIO $ atomically . writeTChan outChan . Left $ Done
