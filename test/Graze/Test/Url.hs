{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.Url
    ( tests
    ) where

import           Data.Either   (isLeft)
import           Data.Foldable (for_)
import qualified Data.Text     as T

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCaseSteps)

import Graze.Url        (Url (Url))
import Graze.Url.Parser (parseUrl, parseRelUrl)


-- parseUrl --------------------------------------------------------------------

absoluteValid :: [(T.Text, Url)]
absoluteValid =
    [ ( "http://www.example.com"
      , Url "http:" "//www.example.com" "/"
      )
    , ( "http://www.example.com/a/b/c"
      , Url "http:" "//www.example.com" "/a/b/c"
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

base :: Url
base = Url "https:" "//a" "/b/c/d?q"

-- Examples from RFC 1808, section 5.1.
relativeValid :: [(T.Text, Url)]
relativeValid =
    [ ("g"      , Url "https:" "//a" "/b/c/g" )
    , ("./g"    , Url "https:" "//a" "/b/c/g" )
    , ("g/"     , Url "https:" "//a" "/b/c/g/")
    , ("/g"     , Url "https:" "//a" "/g"     )
    , ("//g"    , Url "https:" "//g" "/"      )
    , ("."      , Url "https:" "//a" "/b/c/"  )
    , ("./"     , Url "https:" "//a" "/b/c/"  )
    , (".."     , Url "https:" "//a" "/b/"    )
    , ("../"    , Url "https:" "//a" "/b/"    )
    , ("../g"   , Url "https:" "//a" "/b/g"   )
    , ("../.."  , Url "https:" "//a" "/"      )
    , ("../../" , Url "https:" "//a" "/"      )
    , ("../../g", Url "https:" "//a" "/g"     )
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
tests = testGroup "Url" [testParseUrl, testParseRelUrl]
