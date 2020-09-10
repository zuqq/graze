module Main
    ( main
    ) where

import qualified Data.Text as T (pack)

import Options.Applicative

import Graze.HttpUrl (parseUrl)
import Graze         (Config (Config), run)


main :: IO ()
main = do
    let parser = Config
            <$> argument
                (eitherReader $ parseUrl . T.pack)
                (metavar "URL" <> help "URL to start at")
            <*> argument
                str
                (metavar "FOLDER" <> help "Download folder")
            <*> option
                auto
                (long "depth"
                    <> metavar "d"
                    <> value 3
                    <> showDefault
                    <> help "Depth of the search")
            <*> option
                auto
                (long "threads"
                    <> metavar "n"
                    <> value 10
                    <> showDefault
                    <> help "Number of threads")
    config <- execParser $ info (parser <**> helper) fullDesc
    run config
