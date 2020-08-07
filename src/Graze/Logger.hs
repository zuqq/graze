{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Graze.Logger
    ( Chans (Chans)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import qualified Data.ByteString.Char8        as C
import           System.IO                    (stderr)

import Graze.Messages (LogCommand (..))


newtype Chans = Chans {inbox :: TChan LogCommand}

run :: Chans -> IO ()
run Chans {..} = loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopLogging -> return ()
        Log message -> do
            C.hPutStrLn stderr message
            loop
