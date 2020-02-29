{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text          as T (pack)
import           System.Environment (getArgs)

import Graze.HttpUrl.Parser (parse)
import Graze.Runners        (Config (..), run)


main :: IO ()
main = getArgs >>= \case
    [s] -> case (parse . T.pack) s of
        Left _     -> putStrLn "malformed URL" >> usage
        Right base -> run Config
            { cWorkers  = 10
            , cDepth    = 3
            , cBase     = base
            , cFolder   = "download"
            , cDatabase = "db.csv"
            }
    _   -> usage
  where
    usage = putStrLn "usage: graze URL"
