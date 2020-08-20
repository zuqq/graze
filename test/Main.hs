module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Graze.Test.HttpUrl as HttpUrl
import qualified Graze.Test.Links   as Links
import qualified Graze.Test.Robots  as Robots


main :: IO ()
main = defaultMain $ testGroup "Graze" [HttpUrl.tests, Links.tests, Robots.tests]
