{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Logger
    ( runLogger
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (readTBQueue, writeTBQueue)
import qualified Data.Text                      as T (unpack)
import           System.IO                      (hPutStrLn, stderr)

import Graze.Types


-- | Logs the URLs of downloaded pages to 'stderr'.
runLogger :: Queues -> IO ()
runLogger Queues {..} = loop
  where
    loop = (atomically . readTBQueue $ loggerQueue) >>= \case
        StopLogging -> atomically $ writeTBQueue loggerQueue StopLogging
        Log message -> do
            hPutStrLn stderr . T.unpack $ message
            loop
