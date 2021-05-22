{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.HTMLSpec (spec) where

-- For the 'Monoid' instance.
import Data.Set ()
import Data.Maybe (fromJust)
import Test.Hspec

import qualified Data.Set as Set

import Graze.HTML
import Graze.URI

parseLinksSpec :: Spec
parseLinksSpec = do
    let base = fromJust (parseURI "http://www.example.com/")

    it "handles lack of input gracefully" do
        parseLinks base "" `shouldBe` mempty

    let links = Set.singleton base {uriPath = "/a"}

    it "finds links with no quotes" do
        parseLinks base "<!DOCTYPE html><body><a href=a>a</a></body>"
            `shouldBe` links

    it "finds links with single quotes" do
        parseLinks base "<!DOCTYPE html><body><a href='a'>a</a></body>"
            `shouldBe` links

    it "finds links with double quotes" do
        parseLinks base "<!DOCTYPE html><body><a href=\"a\">a</a></body>"
            `shouldBe` links

    it "ignores malformed links" do
        parseLinks base "<!DOCTYPE html><body><ahref=a>a</a></body>"
            `shouldBe` mempty

    it "decodes links" do
        let s =
                "<!DOCTYPE html>                                 \n\
                \<body>                                          \n\
                \    <a href=\"?how=down&amp;goto=news\">down</a>\n\
                \    <a href=\"?how=up&amp;goto=news\">up</a>    \n\
                \</body>                                         \n"

        let expected =
                Set.fromList
                    [ base {uriQuery = "?how=down&goto=news"}
                    , base {uriQuery = "?how=up&goto=news"}
                    ]

        parseLinks base s `shouldBe` expected

spec :: Spec
spec = describe "parseLinks" parseLinksSpec
