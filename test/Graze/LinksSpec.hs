{-# LANGUAGE OverloadedStrings #-}

module Graze.LinksSpec (spec) where

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, shouldBe, specify)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashSet as HashSet (fromList)

import Graze.Url
import Graze.Links

valid :: [(Url, Lazy.ByteString, [Url])]
valid =
    [ ( Url "http:" "//www.example.com" "/"
      , "<!DOCTYPE html><body><a href=a>a</a></body>"
      , [Url "http:" "//www.example.com" "/a"]
      )
    , ( Url "http:" "//www.example.com" "/"
      , "<!DOCTYPE html><body><a href='a'>a</a></body>"
      , [Url "http:" "//www.example.com" "/a"]
      )
    , ( Url "http:" "//www.example.com" "/"
      , "<!DOCTYPE html><body><a href=\"a\">a</a></body>"
      , [Url "http:" "//www.example.com" "/a"]
      )
    ]

parseLinksSpec :: Spec
parseLinksSpec = describe "valid examples" $
    for_ valid $ \(base, bs, urls) ->
        specify (show bs) $
            HashSet.fromList (parseLinks base bs) `shouldBe` HashSet.fromList urls

spec :: Spec
spec = describe "parseLinks" parseLinksSpec
