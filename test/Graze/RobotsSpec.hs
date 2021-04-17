{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.RobotsSpec (spec) where

import Test.Hspec

import Graze.Robots

parseRobotsSpec :: Spec
parseRobotsSpec = do
    describe "valid example" do
        it "parses rules for unhipbot correctly" do
            or (fmap (parseRobots "unhipbot" s) paths)
                `shouldBe` False
        it "parses rules for webcrawler correctly" do
            and (fmap (parseRobots "webcrawler" s) paths)
                `shouldBe` True
        it "parses rules for excite correctly" do
            and (fmap (parseRobots "excite" s) paths)
                `shouldBe` True
        it "parses rules for googlebot correctly" do
            fmap (parseRobots "googlebot" s) paths
                `shouldBe` [False, False, True, False, True]
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
        \Allow: /org/                          \n\
        \Allow: /serv                          \n"
    paths = ["/", "/index.html", "/services/fast.html", "/orgo.gif", "/org/about.html"]

spec :: Spec
spec = describe "parseRobots" parseRobotsSpec
