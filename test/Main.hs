module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Graze.Test.Url    as Url
import qualified Graze.Test.Links  as Links
import qualified Graze.Test.Robots as Robots


main :: IO ()
main = defaultMain $ testGroup "Graze" [Url.tests, Links.tests, Robots.tests]
