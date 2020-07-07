{-# LANGUAGE OverloadedStrings #-}

module Graze.Test.HttpUrl
    ( tests
    ) where

import qualified Data.ByteString as B (ByteString)
import           Data.Either     (isLeft)
import           Data.Foldable   (for_)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCaseSteps)

import Graze.HttpUrl (HttpUrl (HttpUrl), parse, parseRel)

-- parse -----------------------------------------------------------------------

absoluteValid :: [(B.ByteString, HttpUrl)]
absoluteValid =
    [ ( "http://www.example.com"
      , HttpUrl "http:" "//www.example.com" "/"
      )
    , ( "http://www.example.com/a/b/c"
      , HttpUrl "http:" "//www.example.com" "/a/b/c"
      )
    ]

absoluteInvalid :: [(String, B.ByteString)]
absoluteInvalid =
    [ (""         , ""                      )
    , ("mailto:"  , "mailto:tom@example.com")
    , ("http:..." , "http:www.example.com"  )
    , ("http:/...", "http:/www.example.com" )
    ]

testParse :: TestTree
testParse = testCaseSteps "absolute" $ \step -> do
    for_ absoluteValid $ \(x, y) -> do
        step (show x)
        parse x @?= Right y
    for_ absoluteInvalid $ \(s, x) -> do
        step (show s)
        assertBool s (isLeft (parse x))

-- parseRel --------------------------------------------------------------------

base :: HttpUrl
base = HttpUrl "https:" "//a" "/b/c/d?q"

-- Examples from RFC 1808, section 5.1.
relativeValid :: [(B.ByteString, HttpUrl)]
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

testParseRel :: TestTree
testParseRel = testCaseSteps "relative" $ \step -> do
    for_ relativeValid $ \(x, y) -> do
        step (show x)
        parseRel base x @?= Right y
    step (show "")
    parseRel base "" @?= Right base
    step "mailto:"
    assertBool "mailto:" $
        isLeft (parseRel base "mailto:tom@example.com")

-- Interface -------------------------------------------------------------------

tests :: TestTree
tests = testGroup "HttpUrl" [testParse, testParseRel]
