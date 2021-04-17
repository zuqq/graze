{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Main (main) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (try)
import Options.Applicative (Parser, ParserInfo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text
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

    response :: Either HttpException Response
        <- try (get base {uriPath = "/robots.txt"})
    let legal = case response of
            Right (TextPlain, bs) ->
                case Text.decodeUtf8' . Lazy.toStrict $ bs of
                    Left _  -> const True
                    Right s -> parseRobots "graze" s
            _                     -> const True

    results <- newTBMQueueIO threads

    createDirectoryIfMissing True (folder </> "records")

    let loop = do
            mresult <- atomically (readTBMQueue results)
            case mresult of
                Nothing                             -> pure ()
                Just (Result record@Record {..} bs) -> do
                    let name
                            = Char8.unpack
                            . Base16.encode
                            . SHA1.hash
                            . Char8.pack
                            . show
                            $ url
                    Lazy.writeFile
                        (folder </> "records" </> name <.> "json")
                        (Aeson.encode record)
                    Lazy.writeFile (folder </> name) bs
                    hPutStrLn stderr ("Got " <> show url)
                    loop

    concurrently_
        (crawl
            results
            base
            (\uri ->
                    uriAuthority uri == uriAuthority base
                &&  legal (uriPath uri))
            depth
            threads)
        loop

    putStrLn "Done"
