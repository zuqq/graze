{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Main (main) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (try)
import Data.Functor ((<&>))
import Options.Applicative (Parser, ParserInfo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Options.Applicative as Options

import Graze.Crawler
import Graze.Http
import Graze.Node
import Graze.Robots
import Graze.SHA1
import Graze.URI

data MainOptions = MainOptions
    { base    :: URI       -- ^ URL to start at.
    , folder  :: FilePath  -- ^ Folder to store the nodes in.
    , depth   :: Int       -- ^ Depth of the search.
    , threads :: Int       -- ^ Number of threads.
    }

mainOptionsParser :: Parser MainOptions
mainOptionsParser =
        MainOptions
    <$> Options.argument
            (Options.maybeReader parseURI)
            (Options.metavar "<base>" <> Options.help "URL to start at.")
    <*> Options.argument
            Options.str
            (   Options.metavar "<folder>"
            <>  Options.help "Folder to store the nodes in."
            )
    <*> Options.option
            Options.auto
            (   Options.long "depth"
            <>  Options.metavar "<depth>"
            <>  Options.value 3
            <>  Options.showDefault
            <>  Options.help "Depth of the search."
            )
    <*> Options.option
            Options.auto
            (   Options.long "threads"
            <>  Options.metavar "<threads>"
            <>  Options.value 10
            <>  Options.showDefault
            <>  Options.help "Number of threads."
            )

mainOptionsParserInfo :: ParserInfo MainOptions
mainOptionsParserInfo =
    Options.info (Options.helper <*> mainOptionsParser) Options.fullDesc

main :: IO ()
main = do
    MainOptions {..} <- Options.execParser mainOptionsParserInfo

    putStrLn ("Crawling " <> show base)

    robots <-
            try (getTextPlain base {uriPath = "/robots.txt"})
        <&> \case
                Left (_ :: GrazeHttpException) -> const True
                Right s -> parseRobots s

    output <- newTBMQueueIO threads

    createDirectoryIfMissing True folder

    let loop =
            atomically (readTBMQueue output) >>= \case
                Nothing -> pure ()
                Just node@Node {..} -> do
                    let name = hash (show nodeLocation)
                    Lazy.writeFile
                        (folder </> name <.> "json")
                        (Aeson.encode node)
                    hPutStrLn stderr ("Got " <> show nodeLocation)
                    loop

    concurrently_
        (crawl
            CrawlerOptions
                { crawlable =
                    \uri ->
                            uriAuthority uri == uriAuthority base
                        &&  robots (uriPath uri)
                , ..
                })
        loop

    putStrLn "Done"
