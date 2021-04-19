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
import Graze.Robots
import Graze.Types
import Graze.URI

data Options = Options
    { base    :: URI       -- ^ URL to start at.
    , folder  :: FilePath  -- ^ Download folder.
    , depth   :: Int       -- ^ Depth of the search.
    , threads :: Int       -- ^ Number of threads.
    }

parser :: Parser Options
parser =
        Options
    <$> Options.argument
            (Options.maybeReader parseURI)
            (Options.metavar "<URL>" <> Options.help "URL to start at")
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
            <>  Options.help "Number of threads"
            )

parserInfo :: ParserInfo Options
parserInfo = Options.info (Options.helper <*> parser) Options.fullDesc

main :: IO ()
main = do
    Options {..} <- Options.execParser parserInfo

    putStrLn ("Crawling " <> show base)

    robots
        <-  try (get ("text" // "plain") "graze" base {uriPath = "/robots.txt"})
        <&> \case
                Left (_ :: HttpException) -> const True
                Right (Just s) -> parseRobots "graze" s
                _ -> const True

    recordQueue <- newTBMQueueIO threads

    createDirectoryIfMissing True folder

    let loop =
            atomically (readTBMQueue recordQueue) >>= \case
                Nothing -> pure ()
                Just record@Record {..} -> do
                    let name
                            = Char8.unpack
                            . Base16.encode
                            . SHA1.hash
                            . Char8.pack
                            . show
                            $ uri
                    Lazy.writeFile
                        (folder </> name <.> "json")
                        (Aeson.encode record)
                    hPutStrLn stderr ("Got " <> show uri)
                    loop

    concurrently_
        (crawl
            recordQueue
            base
            (\uri ->
                    uriAuthority uri == uriAuthority base
                &&  robots (uriPath uri))
            depth
            threads)
        loop

    putStrLn "Done"
