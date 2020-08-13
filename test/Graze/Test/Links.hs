{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Links
    ( tests
    ) where

import           Data.Foldable (for_)
import           Data.Function (on)
import qualified Data.HashSet  as HS (fromList)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Graze.HttpUrl (HttpUrl (HttpUrl))
import Graze.Links   (links)


testLinks :: TestTree
testLinks = testCase "links" $ do
    for_ examples $ \(x, ys) ->
        ((@?=) `on` HS.fromList) (links base $ header <> x <> footer) ys
  where
    base = HttpUrl "http:" "//www.example.com" "/"
    header = "<!DOCTYPE html><body>"
    footer = "</body>"
    xa = "<a href=\"a\">a</a>"
    xb = "<a href=\"a/b\">a/b</a>"
    ya = HttpUrl "http:" "//www.example.com" "/a"
    yb = HttpUrl "http:" "//www.example.com" "/a/b"
    examples =
        [ (xa      , [ya]    )
        , (xb      , [yb]    )
        , (xa <> xb, [ya, yb])
        ]

tests :: TestTree
tests = testGroup "Links" [testLinks]
