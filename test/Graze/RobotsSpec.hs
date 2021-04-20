{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.RobotsSpec (spec) where

import Test.Hspec

import Graze.Robots

parseRobotsSpec :: Spec
parseRobotsSpec = do
    it "parses rules for \"*\" correctly" do
        fmap (parseRobots s) paths
            `shouldBe` expected
    it "applies the most specific rule" do
        let robots = parseRobots s
        robots "/org"
            `shouldBe` False
        robots "/org/"
            `shouldBe` True
  where
    s = "# /robots.txt for http://www.fict.org/\n\
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
    paths =
        [ "/"
        , "/index.html"
        , "/server.html"
        , "/services/fast.html"
        , "/services/slow.html"
        , "/orgo.gif"
        , "/org/about.html"
        , "/org/plans.html"
        , "/~jim/mak.html"
        , "/~mak/jim.html"
        ]
    expected = [False, False, True, True, True, False, True, False, False, True]

spec :: Spec
spec = describe "parseRobots" parseRobotsSpec
