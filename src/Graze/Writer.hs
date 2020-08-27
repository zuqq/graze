{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer
    ( Chans (Chans)
    , Config (Config)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import qualified Data.ByteString.Lazy         as BL (writeFile)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((<.>), (</>))

import Data.Aeson (encode)

import Graze.HttpUrl  (hashUrl)
import Graze.Messages (Job (..), Record (..), WriteCommand (..))


newtype Config = Config {folder :: FilePath}

newtype Chans = Chans {inbox :: TChan WriteCommand}

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    createDirectoryIfMissing True bytes
    createDirectoryIfMissing True json
    loop
  where
    bytes = folder </> "bytes"
    json  = folder </> "json"
    loop  = atomically (readTChan inbox) >>= \case
        StopWriting       -> return ()
        Write record body -> do
            let name = hashUrl . rUrl $ record
            BL.writeFile (bytes </> name) body
            BL.writeFile (json </> name <.> "json") (encode record)
            loop
