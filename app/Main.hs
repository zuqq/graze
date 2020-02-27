{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graze.HttpUrl (HttpUrl (HttpUrl))
import Graze.Runners (Config (..), run)


main :: IO ()
main = run Config
    { cWorkers  = 10
    , cDepth    = 3
    , cBase     = HttpUrl "http:" "//blog.fefe.de" "/"
    , cFolder   = "download"
    , cDatabase = "db.csv"
    }
