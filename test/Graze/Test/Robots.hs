{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Robots
    ( tests
    ) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Foldable              (for_)
import           Data.Functor               ((<&>))
import qualified Data.Text                  as T

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCaseSteps)

import Graze.Robots (parseRobots)


content :: BLC.ByteString
content = BLC.unlines
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

paths :: [T.Text]
paths =
    [ "/"
    , "/index.html"
    , "/services/fast.html"
    , "/orgo.gif"
    , "/org/about.html"
    ]

cases :: [(T.Text, [Bool])]
cases =
    [ ("unhipbot"  , [False, False, False, False, False])
    , ("webcrawler", [True , True , True , True , True ])
    , ("excite"    , [True , True , True , True , True ])
    , ("googlebot" , [False, False, True , False, True ])
    ]

tests :: TestTree
tests = testGroup "Robots" $
    cases <&> \(ua, results) ->
        let rs = parseRobots ua content
        in testCaseSteps (T.unpack ua) $ \step ->
            for_ (zip paths results) $ \(path, result) -> do
                step (T.unpack path)
                rs path @?= result
