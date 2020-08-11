{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Links
    ( tests
    ) where

import qualified Data.HashSet     as H (fromList)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit ((@?=), testCase)

import Graze.HttpUrl (HttpUrl (HttpUrl))
import Graze.Links   (links)

testLinks :: TestTree
testLinks = testCase "links" $ do
    H.fromList (links base $ header <> a <> footer)
        @?= H.fromList [HttpUrl "http:" "//www.example.com" "/a"]
    H.fromList (links base $ header <> b <> footer)
        @?= H.fromList [HttpUrl "http:" "//www.example.com" "/a/b"]
    H.fromList (links base $ header <> haskell <> footer)
        @?= H.fromList [HttpUrl "http:" "//www.haskell.org" "/"]
    H.fromList (links base $ header <> a <> b <> haskell <> footer)
        @?= H.fromList
                [ HttpUrl "http:" "//www.example.com" "/a"
                , HttpUrl "http:" "//www.example.com" "/a/b"
                , HttpUrl "http:" "//www.haskell.org" "/"
                ]
  where
    base = HttpUrl "http:" "//www.example.com" "/"
    header = "<!DOCTYPE html><body>"
    footer = "</body>"
    a = "<a href=\"a\">a</a>"
    b = "<a href=\"a/b\">b</a>"
    haskell = "<a href=\"http://www.haskell.org\">haskell</a>"

tests :: TestTree
tests = testGroup "Links" [testLinks]
