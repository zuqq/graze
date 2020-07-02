{-# LANGUAGE OverloadedStrings #-}

import Data.Either   (isLeft)
import Data.Foldable (for_)

import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

import Graze.HttpUrl (HttpUrl (HttpUrl), parse, parseRel)
import Graze.Links   (links)


parseTests :: TestTree
parseTests = testCase "parse" $ do
    let valid =
            [ ( "http://www.example.com"
              , HttpUrl "http:" "//www.example.com" "/"
              )
            , ( "http://www.example.com/a/b/c"
              , HttpUrl "http:" "//www.example.com" "/a/b/c"
              )
            ]
    for_ valid $ \(x, y) -> parse x @?= Right y
    let invalid =
            [ ("\"\""     , ""                      )
            , ("mailto:"  , "mailto:tom@example.com")
            , ("http:..." , "http:www.example.com"  )
            , ("http:/...", "http:/www.example.com" )
            ]
    for_ invalid $ \(s, x) -> assertBool s (isLeft (parse x))

parseRelTests :: TestTree
parseRelTests = testCase "parseRel" $ do
    -- Examples from RFC 1808, section 5.1.
    let valid =
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
    for_ valid $ \(x, y) -> parseRel base x @?= Right y
    assertBool "mailto:" $
        isLeft (parseRel base "mailto:tom@example.com")
  where
    base = HttpUrl "https:" "//a" "/b/c/d?q"

urlParsing :: TestTree
urlParsing = testGroup "URL parsing" [parseTests, parseRelTests]

linksTests :: TestTree
linksTests = testCase "links" $ do
    links base (header <> a <> footer)
        @?= [HttpUrl "http:" "//www.example.com" "/a"]
    links base (header <> b <> footer)
        @?= [HttpUrl "http:" "//www.example.com" "/a/b"]
    links base (header <> haskell <> footer)
        @?= [HttpUrl "http:" "//www.haskell.org" "/"]
  where
    base = HttpUrl "http:" "//www.example.com" "/"
    header = "<!doctype html><body>"
    footer = "</body>"
    a = "<a href=\"a\">a</a>"
    b = "<a href=\"a/b\">b</a>"
    haskell = "<a href=\"http://www.haskell.org\">haskell</a>"

linkParsing :: TestTree
linkParsing = testGroup "link parsing" [linksTests]

main :: IO ()
main = defaultMain $ testGroup "root" [urlParsing, linkParsing]
