{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.CrawlerSpec (spec) where

import Control.Concurrent.Async (cancel, concurrently, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (bracket)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (close, withSocketsDo)
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.Hspec

import qualified Data.Set as Set

import Graze.Crawler
import Graze.Node
import Graze.URI

-- Running the @IO ()@ action tells the caller that the server has started.
runSettingsStarted :: Settings -> IO () -> Application -> IO ()
runSettingsStarted settings started application =
    -- Derived from 'Network.Wai.Handler.Warp.runSettings'; doesn't set the
    -- @CloseOnExec@ flag on the socket.
    withSocketsDo
        (bracket
            (bindPortTCP (getPort settings) (getHost settings))
            close
            \socket -> do
                started
                runSettingsSocket settings socket application)

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = go
  where
    go = do
        u <- f
        case u of
            Nothing -> pure []
            Just x -> do
                xs <- go
                pure (x : xs)

crawlSpec :: Spec
crawlSpec = do
    let base = fromJust (parseURI "http://127.0.0.1:8080/index.html")

    let crawlable uri = uriAuthority uri == uriAuthority base

    let depth = 3

    let threads = 10

    let example = do
            serverStarted <- newEmptyMVar

            output <- newTBMQueueIO threads

            withAsync
                (runSettingsStarted
                    (setPort 8080 defaultSettings)
                    (putMVar serverStarted ())
                    (staticApp (defaultFileServerSettings "./test/example")))
                \server -> do
                    takeMVar serverStarted
                    (_, nodes) <-
                        concurrently
                            (crawl CrawlerOptions {..})
                            (unfoldM (atomically (readTBMQueue output)))
                    cancel server
                    pure nodes

    let a = base {uriPath = "/a.html"}
        o = fromJust (parseURI "http://www.example.com")

    let b = base {uriPath = "/b.html"}
        u = base {uriPath = "/u.html"}

    let c = base {uriPath = "/c.html"}
        s = a {uriScheme = "https:"}
        t = a {uriFragment = "#top"}

    let d = base {uriPath = "/d.html"}

    let expected =
            Set.fromList
                [ Node base base (Set.fromList [a, o])
                , Node base a (Set.fromList [b, u])
                , Node a b (Set.fromList [c, s, t])
                , Node b c (Set.fromList [c, d])
                ]

    it "crawls the example correctly" do
        fmap Set.fromList example `shouldReturn` expected

spec :: Spec
spec = describe "crawl" crawlSpec
