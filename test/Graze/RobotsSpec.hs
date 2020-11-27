{-# LANGUAGE OverloadedStrings #-}

module Graze.RobotsSpec
    ( spec
    ) where

import Data.Foldable (for_)
import qualified Data.Text as T
import Test.Hspec (Spec, describe, shouldBe, specify)

import Graze.Robots


content :: T.Text
content = T.unlines
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

cases :: [(UserAgent, [Bool])]
cases =
    [ ("unhipbot"  , [False, False, False, False, False])
    , ("webcrawler", [True , True , True , True , True ])
    , ("excite"    , [True , True , True , True , True ])
    , ("googlebot" , [False, False, True , False, True ])
    ]

parseRobotsSpec :: Spec
parseRobotsSpec = describe "valid examples" $
    for_ cases $ \(userAgent, results) ->
        describe (show userAgent) $
            let legal = parseRobots userAgent content
            in for_ (zip paths results) $ \(path, result) ->
                specify (show path) $ legal path `shouldBe` result

spec :: Spec
spec = describe "parseRobots" parseRobotsSpec
