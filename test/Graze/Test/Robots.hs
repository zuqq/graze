{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Robots
    ( tests
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Foldable         (for_)
import           Data.Functor          ((<&>))

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCaseSteps)

import qualified Graze.Robots as Robots


content :: C8.ByteString
content = C8.unlines
    [ "User-agent: bot"
    , "Disallow: /"
    , ""
    , "User-agent: *"
    , "Disallow: /a/b"
    , "Disallow: /tmp/"
    ]

paths :: [C8.ByteString]
paths =
    [ "/"
    , "/index.html"
    , "/secret.html"
    , "/a/b"
    , "/a/b/c"
    , "/tmp"
    , "/tmp/"
    ]

cases :: [(C8.ByteString, [Bool])]
cases =
    [ ("bot", [False, False, False, False, False, False, False])
    , ("*"  , [True , True , True , False, False, True , False])
    , ("tom", [True , True , True , False, False, True , False])
    ]

tests :: TestTree
tests = testGroup "Robots" $
    cases <&> \(ua, results) ->
        let robots = Robots.parse ua content
        in testCaseSteps (C8.unpack ua) $ \step ->
            for_ (zip paths results) $ \(path, result) -> do
                step (C8.unpack path)
                path `Robots.allowedBy` robots @?= result
