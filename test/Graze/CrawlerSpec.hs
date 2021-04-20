{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.CrawlerSpec (spec) where

import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (bracket)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (close, withSocketsDo)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.Hspec

import qualified Data.Set as Set

import Graze.Crawler
import Graze.Types
import Graze.URI

base :: URI
base = fromJust (parseURI "http://127.0.0.1:8080/index.html")

link :: URI
link = base {uriPath = "/a.html"}

expected :: Set Record
expected =
    Set.fromList
        [ Record base base (Set.singleton link)
        , Record base link mempty
        ]

crawl_ :: IO (Set Record)
crawl_ = do
    -- This MVar is used to tell the parent thread that the server has bound to
    -- the socket. If we don't do this, then bad interleaving can cause the
    -- test to fail.
    serverStarted <- newEmptyMVar

    let serve = do
            let application =
                    staticApp (defaultFileServerSettings "./test/example")
                settings    = setPort 8080 defaultSettings
            -- Reimplementation of 'Network.Wai.Handler.Warp.run', except that
            -- it doesn't set @CloseOnExit@.
            withSocketsDo
                (bracket
                    (bindPortTCP (getPort settings) (getHost settings))
                    close
                    (\socket -> do
                        putMVar serverStarted ()
                        runSettingsSocket settings socket application))

    recordQueue <- newTBMQueueIO 1

    let loop records =
            atomically (readTBMQueue recordQueue) >>= \case
                Nothing -> pure records
                Just record -> loop (Set.insert record records)

    withAsync serve (\server -> do
        takeMVar serverStarted
        withAsync (crawl recordQueue base (const True) 1 1) (\crawler -> do
            records <- loop mempty
            cancel server
            pure records))

crawlSpec :: Spec
crawlSpec = it "crawls the example correctly" (crawl_ `shouldReturn` expected)

spec :: Spec
spec = describe "crawl" crawlSpec
