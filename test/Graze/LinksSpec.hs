{-# LANGUAGE OverloadedStrings #-}

module Graze.LinksSpec
    ( spec
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import qualified Data.HashSet as HS (fromList)
import Test.Hspec (Spec, describe, shouldBe, specify)

import Graze.Url
import Graze.Links


valid :: [(Url, BL.ByteString, [Url])]
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
            HS.fromList (parseLinks base bs) `shouldBe` HS.fromList urls

spec :: Spec
spec = describe "parseLinks" parseLinksSpec
