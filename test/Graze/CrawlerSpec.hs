{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.CrawlerSpec (spec) where

import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (bracket)
import Data.Maybe (fromJust)
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (close, withSocketsDo)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.Hspec

import qualified Data.Set as Set

import Graze.Crawler
import Graze.Record
import Graze.URI

crawlSpec :: Spec
crawlSpec = do
    it "crawls the example correctly" do
        example
            `shouldReturn` expected
  where
    base = fromJust (parseURI "http://127.0.0.1:8080/index.html")
    a = base {uriPath = "/a.html"}
    b = base {uriPath = "/b.html"}
    expected =
        Set.fromList
            [ Record base base (Set.singleton a)
            , Record base a (Set.singleton b)
            , Record a b (Set.singleton a)
            ]
    example = do
        let threads = 10
            depth   = 3

        -- This MVar is used to tell the parent thread that the server has
        -- bound to 8080. If we don't do this, then bad interleaving can cause
        -- the test to fail.
        serverStarted <- newEmptyMVar

        let serve = do
                let application =
                        staticApp (defaultFileServerSettings "./test/example")
                    settings    = setPort 8080 defaultSettings
                -- Reimplementation of 'Network.Wai.Handler.Warp.run', except
                -- that it doesn't set @CloseOnExit@.
                withSocketsDo
                    (bracket
                        (bindPortTCP (getPort settings) (getHost settings))
                        close
                        (\socket -> do
                            putMVar serverStarted ()
                            runSettingsSocket settings socket application))

        recordQueue <- newTBMQueueIO threads

        let loop records =
                atomically (readTBMQueue recordQueue) >>= \case
                    Nothing -> pure records
                    Just record -> loop (Set.insert record records)

        withAsync serve (\server -> do
            takeMVar serverStarted
            withAsync
                (crawl
                    CrawlerOptions
                        { crawlable =
                            (\uri -> uriAuthority uri == uriAuthority base)
                        , ..
                        })
                (\crawler -> do
                    records <- loop mempty
                    cancel server
                    pure records))

spec :: Spec
spec = describe "crawl" crawlSpec
