{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.HttpUrl
    ( tests
    ) where

import           Data.Either   (isLeft)
import           Data.Foldable (for_)
import qualified Data.Text     as T

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCaseSteps)

import Graze.HttpUrl (HttpUrl (HttpUrl), parseUrl, parseRelUrl)


-- parseUrl --------------------------------------------------------------------

absoluteValid :: [(T.Text, HttpUrl)]
absoluteValid =
    [ ( "http://www.example.com"
      , HttpUrl "http:" "//www.example.com" "/"
      )
    , ( "http://www.example.com/a/b/c"
      , HttpUrl "http:" "//www.example.com" "/a/b/c"
      )
    ]

absoluteInvalid :: [(String, T.Text)]
absoluteInvalid =
    [ (""         , ""                      )
    , ("mailto:"  , "mailto:tom@example.com")
    , ("http:..." , "http:www.example.com"  )
    , ("http:/...", "http:/www.example.com" )
    ]

testParseUrl :: TestTree
testParseUrl = testCaseSteps "absolute" $ \step -> do
    for_ absoluteValid $ \(x, y) -> do
        step (show x)
        parseUrl x @?= Right y
    for_ absoluteInvalid $ \(s, x) -> do
        step (show s)
        assertBool s (isLeft (parseUrl x))

-- parseRelUrl -----------------------------------------------------------------

base :: HttpUrl
base = HttpUrl "https:" "//a" "/b/c/d?q"

-- Examples from RFC 1808, section 5.1.
relativeValid :: [(T.Text, HttpUrl)]
relativeValid =
    [ ("g"      , HttpUrl "https:" "//a" "/b/c/g" )
    , ("./g"    , HttpUrl "https:" "//a" "/b/c/g" )
    , ("g/"     , HttpUrl "https:" "//a" "/b/c/g/")
    , ("/g"     , HttpUrl "https:" "//a" "/g"     )
    , ("//g"    , HttpUrl "https:" "//g" "/"      )
    , ("."      , HttpUrl "https:" "//a" "/b/c/"  )
    , ("./"     , HttpUrl "https:" "//a" "/b/c/"  )
    , (".."     , HttpUrl "https:" "//a" "/b/"    )
    , ("../"    , HttpUrl "https:" "//a" "/b/"    )
    , ("../g"   , HttpUrl "https:" "//a" "/b/g"   )
    , ("../.."  , HttpUrl "https:" "//a" "/"      )
    , ("../../" , HttpUrl "https:" "//a" "/"      )
    , ("../../g", HttpUrl "https:" "//a" "/g"     )
    ]

testParseRelUrl :: TestTree
testParseRelUrl = testCaseSteps "relative" $ \step -> do
    for_ relativeValid $ \(x, y) -> do
        step (show x)
        parseRelUrl base x @?= Right y
    step (show "")
    parseRelUrl base "" @?= Right base
    step "mailto:"
    assertBool "mailto:" $
        isLeft (parseRelUrl base "mailto:tom@example.com")

-- Interface -------------------------------------------------------------------

tests :: TestTree
tests = testGroup "HttpUrl" [testParseUrl, testParseRelUrl]
