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
    createDirectoryIfMissing True (folder </> "records")
    loop
  where
    loop  = (atomically . readTBQueue $ writerQueue) >>= \case
        StopWriting       -> return ()
        Write record body -> do
            let name = hashUrl . url $ record
            BL.writeFile (folder </> "records" </> name <.> "json") (encode record)
            BL.writeFile (folder </> name) body
            loop
