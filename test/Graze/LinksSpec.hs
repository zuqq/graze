{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.LinksSpec (spec) where

import Test.Hspec

import Graze.Links
import Graze.Url

parseLinksSpec :: Spec
parseLinksSpec = do
    describe "valid examples" do
        it "handles lack of input gracefully" do
            parseLinks base ""
                `shouldBe` []
        it "finds links with no quotes" do
            parseLinks base "<!DOCTYPE html><body><a href=a>a</a></body>"
                `shouldBe` links
        it "finds links with single quotes" do
            parseLinks base "<!DOCTYPE html><body><a href='a'>a</a></body>"
                `shouldBe` links
        it "finds links with double quotes" do
            parseLinks base "<!DOCTYPE html><body><a href=\"a\">a</a></body>"
                `shouldBe` links
    describe "invalid examples" do
        it "ignores malformed links" do
            parseLinks base "<!DOCTYPE html><body><ahref=a>a</a></body>"
                `shouldBe` []
  where
    base = Url "http:" "//www.example.com" "/"
    links = [Url "http:" "//www.example.com" "/a"]

spec :: Spec
spec = describe "parseLinks" parseLinksSpec
