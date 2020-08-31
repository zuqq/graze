{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Logger
    ( runLogger
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import qualified Data.Text                    as T (unpack)
import           System.IO                    (hPutStrLn, stderr)

import Graze.Types (Chans (..), LoggerCommand (..))


runLogger :: Chans -> IO ()
runLogger Chans {..} = loop
  where
    loop = (atomically . readTChan $ loggerChan) >>= \case
        StopLogging -> return ()
        Log message -> do
            hPutStrLn stderr . T.unpack $ message
            loop
