{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graze.HttpUrl (HttpUrl(..))
import Graze.Runner


main :: IO ()
main = run Config
    { cWorkers  = 10
    , cDepth    = 3
    , cBase     = HttpUrl "http" "blog.fefe.de" "/"
    , cFolder   = "download"
    , cDatabase = "db.csv"
    }
