{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Links
    ( tests
    ) where

import qualified Data.HashSet     as HS (fromList)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit ((@?=), testCase)

import Graze.HttpUrl (HttpUrl (HttpUrl))
import Graze.Links   (links)

testLinks :: TestTree
testLinks = testCase "links" $ do
    HS.fromList (links base $ header <> a <> footer)
        @?= HS.fromList [HttpUrl "http:" "//www.example.com" "/a"]
    HS.fromList (links base $ header <> b <> footer)
        @?= HS.fromList [HttpUrl "http:" "//www.example.com" "/a/b"]
    HS.fromList (links base $ header <> haskell <> footer)
        @?= HS.fromList [HttpUrl "http:" "//www.haskell.org" "/"]
    HS.fromList (links base $ header <> a <> b <> haskell <> footer)
        @?= HS.fromList
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
