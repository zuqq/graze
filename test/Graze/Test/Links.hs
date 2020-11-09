{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Links
    ( tests
    ) where

import           Data.Foldable (for_)
import           Data.Function (on)
import qualified Data.HashSet  as HS (fromList)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Graze.Url (Url (Url))
import Graze.Links   (parseLinks)


testParseLinks :: TestTree
testParseLinks = testCase "parseLinks" $ do
    for_ examples $ \(x, ys) ->
        ((@?=) `on` HS.fromList) (parseLinks base $ header <> x <> footer) ys
  where
    base = Url "http:" "//www.example.com" "/"
    header = "<!DOCTYPE html><body>"
    footer = "</body>"
    a   = "<a href=a>a</a>"
    a'  = "<a href='a'>a</a>"
    a'' = "<a href=\"a\">a</a>"
    y   = Url "http:" "//www.example.com" "/a"
    examples = [(a, [y]), (a', [y]), (a'', [y])]

tests :: TestTree
tests = testGroup "Links" [testParseLinks]
