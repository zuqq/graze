module Main
    ( main
    ) where

import qualified Data.Text as T (pack)

import Options.Applicative

import Graze.HttpUrl (parseUrl)
import Graze.Main    (Config (Config), run)


confParser :: Parser Config
confParser = Config
    <$> option auto
        (long "depth"
        <> metavar "d"
        <> value 3
        <> help "Depth of the search"
        <> showDefault)
    <*> option auto
        (long "threads"
        <> metavar "n"
        <> value 10
        <> help "Number of threads"
        <> showDefault)
    <*> argument (eitherReader $ parseUrl . T.pack)
        (metavar "URL"
        <> help "URL to start at")
    <*> argument str
        (metavar "FOLDER"
        <> help "Download folder")

confInfo :: ParserInfo Config
confInfo = info (confParser <**> helper) fullDesc

main :: IO ()
main = execParser confInfo >>= run
