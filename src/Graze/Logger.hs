{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Logger
    ( runLogger
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.Text                      as T (unpack)
import           System.IO                      (hPutStrLn, stderr)

import Graze.Types (Queues (..), LoggerCommand (..))


runLogger :: Queues -> IO ()
runLogger Queues {..} = loop
  where
    loop = (atomically . readTBQueue $ loggerQueue) >>= \case
        StopLogging -> return ()
        Log message -> do
            hPutStrLn stderr . T.unpack $ message
            loop
