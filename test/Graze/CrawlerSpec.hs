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
import Graze.Node
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
    c = base {uriPath = "/c.html"}
    external = fromJust (parseURI "http://www.example.com")
    expected =
        Set.fromList
            [ Node base base (Set.fromList [a, external])
            , Node base a (Set.fromList [b, c])
            , Node
                a
                b
                (Set.fromList
                    [a, a {uriFragment = "#top"}, a {uriScheme = "https:"}])
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

        output <- newTBMQueueIO threads

        let loop nodes =
                atomically (readTBMQueue output) >>= \case
                    Nothing -> pure nodes
                    Just node -> loop (Set.insert node nodes)

        withAsync serve (\server -> do
            takeMVar serverStarted
            withAsync
                (crawl
                    CrawlerOptions
                        { crawlable =
                            \uri -> uriAuthority uri == uriAuthority base
                        , ..
                        })
                (\crawler -> do
                    nodes <- loop mempty
                    cancel server
                    pure nodes))

spec :: Spec
spec = describe "crawl" crawlSpec
