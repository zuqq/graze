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
import           Data.Aeson                   (encode)
import qualified Data.ByteString.Lazy         as BL (writeFile)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((<.>), (</>))

import Graze.HttpUrl  (hash)
import Graze.Messages (Job (..), Record (..), WriteCommand (..))


newtype Config = Config {folder :: FilePath}

newtype Chans = Chans {inbox :: TChan WriteCommand}

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    createDirectoryIfMissing True folder
    loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopWriting  -> return ()
        Write record -> do
            let name = hash . jUrl . rJob $ record
            BL.writeFile (folder </> name) (rBody record)
            BL.writeFile (folder </> name <.> "json") (encode record)
            loop
