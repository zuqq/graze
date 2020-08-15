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
    a   = "<a href=a>a</a>"
    a'  = "<a href='a'>a</a>"
    a'' = "<a href=\"a\">a</a>"
    y   = HttpUrl "http:" "//www.example.com" "/a"
    examples = [(a, [y]), (a', [y]), (a'', [y])]

tests :: TestTree
tests = testGroup "Links" [testLinks]
