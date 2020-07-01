{-# LANGUAGE OverloadedStrings #-}

import Data.Either (isLeft)

import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

import Graze.HttpUrl (HttpUrl (HttpUrl), parse, parseRel)
import Graze.Links   (links)


parseTests :: TestTree
parseTests = testCase "parse" $ do
    parse "http://www.example.com"
        @?= Right (HttpUrl "http:" "//www.example.com" "/")
    parse "http://www.example.com/a/b/c"
        @?= Right (HttpUrl "http:" "//www.example.com" "/a/b/c")
    assertBool "\"\"" $
        isLeft (parse "")
    assertBool "mailto:" $
        isLeft (parse "mailto:tom@example.com")
    assertBool "http:..." $
        isLeft (parse "http:www.example.com")
    assertBool "http:/..." $
        isLeft (parse "http:/www.example.com")

parseRelTests :: TestTree
parseRelTests = testCase "parseRel" $ do
    parseRel base "/"
        @?= Right (HttpUrl "https:" "//www.example.com" "/")
    parseRel base "b/c"
        @?= Right (HttpUrl "https:" "//www.example.com" "/a/b/c")
    parseRel base ""
         @?= Right (HttpUrl "https:" "//www.example.com" "/")
    assertBool "mailto:" $
        isLeft (parseRel base "mailto:tom@example.com")
    parseRel base "//www.haskell.org"
        @?= Right (HttpUrl "https:" "//www.haskell.org" "/")
  where
    base = HttpUrl "https:" "//www.example.com" "/a/"

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
