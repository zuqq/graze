{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer
    ( runWriter
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.ByteString.Lazy           as BL
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                ((</>))
import           System.IO                      (IOMode (WriteMode), withBinaryFile)

import Data.Aeson (encode)

import Graze.HttpUrl (hashUrl)
import Graze.Types   (Queues (..), Record (..), WriterCommand (..))


runWriter :: FilePath -> Queues -> IO ()
runWriter folder Queues {..} = do
    let objects = folder </> "objects"
    createDirectoryIfMissing True objects
    withBinaryFile (folder </> "index") WriteMode $ \index -> do
        let loop = (atomically . readTBQueue $ writerQueue) >>= \case
                StopWriting       -> return ()
                Write record body -> do
                    BL.writeFile (objects </> (hashUrl . url) record) body
                    BL.hPut index (encode record)
                    BL.hPut index (BL.singleton 0x0a)  -- From 'BL.putStrLn'.
                    loop
        loop
