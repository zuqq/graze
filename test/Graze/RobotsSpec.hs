{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.RobotsSpec (spec) where

import Data.Foldable (for_)
import Test.Hspec

import Graze.Robots

parseRobotsSpec :: Spec
parseRobotsSpec = do
    let robots =
            parseRobots
                "# /robots.txt for http://www.fict.org/\n\
                \# comments to webmaster@fict.org      \n\
                \                                      \n\
                \User-agent: unhipbot                  \n\
                \Disallow: /                           \n\
                \                                      \n\
                \User-agent: webcrawler                \n\
                \User-agent: excite                    \n\
                \Disallow:                             \n\
                \                                      \n\
                \User-agent: *                         \n\
                \Disallow: /                           \n\
                \Disallow: /org/plans.html             \n\
                \Allow: /org/                          \n\
                \Allow: /serv                          \n\
                \Allow: /~mak                          \n"

    let examples =
            [ ("/", False)
            , ("/index.html", False)
            , ("/server.html", True)
            , ("/services/fast.html", True)
            , ("/services/slow.html", True)
            , ("/orgo.gif", False)
            , ("/org/about.html", True)
            , ("/org/plans.html", False)
            , ("/~jim/mak.html", False)
            , ("/~mak/jim.html", True)
            ]

    it "parses rules for \"*\" correctly" do
        for_ examples \(x, y) -> robots x `shouldBe` y

    it "applies the most specific rule" do
        robots "/org" `shouldBe` False
        robots "/org/" `shouldBe` True

spec :: Spec
spec = describe "parseRobots" parseRobotsSpec
