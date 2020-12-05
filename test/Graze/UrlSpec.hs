{-# LANGUAGE OverloadedStrings #-}

module Graze.UrlSpec
    ( spec
    )
    where

import Data.Either (isLeft)
import Data.Foldable (for_)
import qualified Data.Text as T (Text)
import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy, specify)

import Graze.Url


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

parseUrlSpec :: Spec
parseUrlSpec = do
    describe "valid examples" $
        for_ absoluteValid $ \(x, y) ->
            specify (show x) $ parseUrl x `shouldBe` Right y
    describe "invalid examples" $
        for_ absoluteInvalid $ \(s, x) ->
            specify (show s) $ parseUrl x `shouldSatisfy` isLeft

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

relativeInvalid :: [(String, T.Text)]
relativeInvalid = [("mailto:", "mailto:tom@example.com")]

parseRelUrlSpec :: Spec
parseRelUrlSpec = do
    describe "valid examples" $ do
        for_ relativeValid $ \(x, y) ->
            specify (show x) $ parseRelUrl base x `shouldBe` Right y
        specify "\"\"" $ parseRelUrl base "" `shouldBe` Right base
    describe "invalid examples" $
        for_ relativeInvalid $ \(s, x) ->
            specify (show s) $ parseRelUrl base x `shouldSatisfy` isLeft

spec :: Spec
spec = do
    describe "parseUrl" parseUrlSpec
    describe "parseRelUrl" parseRelUrlSpec
