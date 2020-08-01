{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Robots
    ( tests
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Foldable         (for_)
import           Data.Functor          ((<&>))

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCaseSteps)

import Graze.Robots (parse)


content :: C8.ByteString
content = C8.unlines
    [ "# /robots.txt for http://www.fict.org/"
    , "# comments to webmaster@fict.org"
    , ""
    , "User-agent: unhipbot"
    , "Disallow: /"
    , ""
    , "User-agent: webcrawler"
    , "User-agent: excite"
    , "Disallow: "
    , ""
    , "User-agent: *"
    , "Disallow: /"
    , "Allow: /org/"
    , "Allow: /serv"
    ]

paths :: [C8.ByteString]
paths =
    [ "/"
    , "/index.html"
    , "/services/fast.html"
    , "/orgo.gif"
    , "/org/about.html"
    ]

cases :: [(C8.ByteString, [Bool])]
cases =
    [ ("unhipbot"  , [False, False, False, False, False])
    , ("webcrawler", [True , True , True , True , True ])
    , ("excite"    , [True , True , True , True , True ])
    , ("googlebot" , [False, False, True , False, True ])
    ]

tests :: TestTree
tests = testGroup "Robots" $
    cases <&> \(ua, results) ->
        let rs = parse ua content
        in testCaseSteps (C8.unpack ua) $ \step ->
            for_ (zip paths results) $ \(path, result) -> do
                step (C8.unpack path)
                rs path @?= result
