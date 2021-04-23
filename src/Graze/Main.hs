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

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Options.Applicative as Options

import Graze.Crawler
import Graze.Http
import Graze.Node
import Graze.Robots
import Graze.URI

data MainOptions = MainOptions
    { base    :: URI       -- ^ URL to start at.
    , folder  :: FilePath  -- ^ Download folder.
    , depth   :: Int       -- ^ Depth of the search.
    , threads :: Int       -- ^ Size of the thread pool.
    }

mainOptionsParser :: Parser MainOptions
mainOptionsParser =
        MainOptions
    <$> Options.argument
            (Options.maybeReader parseURI)
            (Options.metavar "<base>" <> Options.help "URL to start at")
    <*> Options.argument
            Options.str
            (Options.metavar "<folder>" <> Options.help "Download folder")
    <*> Options.option
            Options.auto
            (   Options.long "depth"
            <>  Options.metavar "<depth>"
            <>  Options.value 3
            <>  Options.showDefault
            <>  Options.help "Depth of the search"
            )
    <*> Options.option
            Options.auto
            (   Options.long "threads"
            <>  Options.metavar "<threads>"
            <>  Options.value 10
            <>  Options.showDefault
            <>  Options.help "Size of the thread pool"
            )

mainOptionsParserInfo :: ParserInfo MainOptions
mainOptionsParserInfo =
    Options.info (Options.helper <*> mainOptionsParser) Options.fullDesc

main :: IO ()
main = do
    MainOptions {..} <- Options.execParser mainOptionsParserInfo

    putStrLn ("Crawling " <> show base)

    robots <-
            try (get ("text" // "plain") "graze" base {uriPath = "/robots.txt"})
        <&> \case
                Left (_ :: HttpException) -> const True
                Right (Just s) -> parseRobots s
                _ -> const True

    output <- newTBMQueueIO threads

    createDirectoryIfMissing True folder

    let loop =
            atomically (readTBMQueue output) >>= \case
                Nothing -> pure ()
                Just node@(Node _ location _) -> do
                    let name =
                              Char8.unpack
                            . Base16.encode
                            . SHA1.hash
                            . Char8.pack
                            . show
                            $ location
                    Lazy.writeFile
                        (folder </> name <.> "json")
                        (Aeson.encode node)
                    hPutStrLn stderr ("Got " <> show location)
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
