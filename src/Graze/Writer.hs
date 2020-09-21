{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer
    ( runWriter
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.ByteString.Lazy           as BL (writeFile)
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                ((<.>), (</>))

import Data.Aeson (encode)

import Graze.HttpUrl (hashUrl)
import Graze.Types   (Queues (..), Record (..), WriterCommand (..))


runWriter :: FilePath -> Queues -> IO ()
runWriter folder Queues {..} = do
    createDirectoryIfMissing True json
    createDirectoryIfMissing True bytes
    loop
  where
    json  = folder </> "json"
    bytes = folder </> "bytes"
    loop  = (atomically . readTBQueue $ writerQueue) >>= \case
        StopWriting       -> return ()
        Write record body -> do
            let name = hashUrl . url $ record
            BL.writeFile (json </> name <.> "json") (encode record)
            BL.writeFile (bytes </> name) body
            loop
