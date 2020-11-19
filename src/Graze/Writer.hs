{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Graze.Writer
    ( runWriter
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.ByteString.Lazy           as BL (writeFile)
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                ((<.>), (</>))

import Data.Aeson (encode)

import Graze.Types
import Graze.Url


runWriter :: FilePath -> Queues -> IO ()
runWriter folder Queues {..} = do
    createDirectoryIfMissing True (folder </> "records")
    loop
  where
    loop = (atomically . readTBQueue $ writerQueue) >>= \case
        StopWriting       -> return ()
        Write record body -> do
            let Record _ (hashUrl -> name) _ = record
            BL.writeFile (folder </> "records" </> name <.> "json") (encode record)
            BL.writeFile (folder </> name) body
            loop
