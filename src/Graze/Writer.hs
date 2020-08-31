{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer
    ( runWriter
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import qualified Data.ByteString.Lazy         as BL (writeFile)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((<.>), (</>))

import Data.Aeson (encode)

import Graze.HttpUrl (hashUrl)
import Graze.Types   (Chans (..), Record (..), WriterCommand (..))


runWriter :: FilePath -> Chans -> IO ()
runWriter folder Chans {..} = do
    createDirectoryIfMissing True json
    createDirectoryIfMissing True bytes
    loop
  where
    json  = folder </> "json"
    bytes = folder </> "bytes"
    loop  = (atomically . readTChan $ writerChan) >>= \case
        StopWriting       -> return ()
        Write record body -> do
            let name = hashUrl . url $ record
            BL.writeFile (json </> name <.> "json") (encode record)
            BL.writeFile (bytes </> name) body
            loop
