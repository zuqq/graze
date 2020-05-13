{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T (pack)

import Options.Applicative

import Graze.HttpUrl (parse)
import Graze.Runners (Config (Config), run)


confParser :: Parser Config
confParser = Config
    <$> option auto
        (long "workers"
        <> metavar "n"
        <> value 10
        <> help "Number of worker threads")
    <*> option auto
        (long "depth"
        <> metavar "d"
        <> value 3
        <> help "Depth of the search")
    <*> option auto
        (long "folder"
        <> metavar "f"
        <> value "download"
        <> help "Download folder")
    <*> option auto
        (long "records"
        <> metavar "r"
        <> value "records"
        <> help "Page record file")
    <*> argument (eitherReader $ parse . T.pack)
        (metavar "base"
        <> help "URL to start at")

confInfo :: ParserInfo Config
confInfo = info (confParser <**> helper) fullDesc

main :: IO ()
main = execParser confInfo >>= run
