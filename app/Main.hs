module Main
    ( main
    ) where

import qualified Data.Text as T (pack)

import Options.Applicative

import Graze (Config (Config), parseUrl, run)

parser :: Parser Config
parser = Config
    <$> argument
        (eitherReader $ parseUrl . T.pack)
        (metavar "<URL>" <> help "URL to start at")
    <*> argument
        str
        (metavar "<folder>" <> help "Download folder")
    <*> option
        auto
        (long "depth"
            <> metavar "<depth>"
            <> value 3
            <> showDefault
            <> help "Depth of the search")
    <*> option
        auto
        (long "threads"
            <> metavar "<threads>"
            <> value 10
            <> showDefault
            <> help "Number of threads")

main :: IO ()
main = do
    config <- execParser $ info (parser <**> helper) fullDesc
    run config
