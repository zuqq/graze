{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.HTMLSpec (spec) where

import Data.Set ()
import Data.Maybe (fromJust)
import Test.Hspec

import qualified Data.Set as Set

import Graze.HTML
import Graze.URI

parseLinksSpec :: Spec
parseLinksSpec = do
    describe "valid examples" do
        it "handles lack of input gracefully" do
            parseLinks base ""
                `shouldBe` mempty
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
                `shouldBe` mempty
  where
    base = fromJust (parseURI "http://www.example.com/")
    links = Set.fromList [fromJust (parseURI "http://www.example.com/a")]

spec :: Spec
spec = describe "parseLinks" parseLinksSpec
