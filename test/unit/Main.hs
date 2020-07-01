{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty       (TestTree, defaultMain)
import Test.Tasty.HUnit ((@?=), testCase)

import Graze.HttpUrl (HttpUrl (HttpUrl))
import Graze.Links   (links)


linksTests :: TestTree
linksTests = testCase "links" $ do
    links base (header <> a <> footer)
        @?= [HttpUrl "http:" "//www.example.com" "/a"]
    links base (header <> b <> footer)
        @?= [HttpUrl "http:" "//www.example.com" "/a/b"]
    links base (header <> haskell <> footer)
        @?= [HttpUrl "http:" "//www.haskell.org" "/"]
  where
    base = HttpUrl "http:" "//www.example.com" "/"
    header = "<!doctype html><body>"
    footer = "</body>"
    a = "<a href=\"a\">a</a>"
    b = "<a href=\"a/b\">b</a>"
    haskell = "<a href=\"http://www.haskell.org\">haskell</a>"

main :: IO ()
main = defaultMain linksTests
