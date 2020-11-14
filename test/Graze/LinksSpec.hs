{-# LANGUAGE OverloadedStrings #-}

module Graze.LinksSpec
    ( spec
    ) where

import           Data.Foldable (for_)
import qualified Data.HashSet  as HS (fromList)
import qualified Data.Text     as T (Text, unpack)

import Test.Hspec (Spec, describe, shouldBe, specify)

import Graze.Url   (Url (Url))
import Graze.Links (parseLinks)


valid :: [(Url, T.Text, [Url])]
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
    for_ valid $ \(base, s, urls) ->
        specify (T.unpack s) $
            HS.fromList (parseLinks base s) `shouldBe` HS.fromList urls

spec :: Spec
spec = describe "parseLinks" parseLinksSpec
