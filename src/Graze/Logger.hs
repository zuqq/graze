{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Logger
    ( Chans (Chans)
    , Config (Config)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import qualified Data.ByteString.Char8        as C8

import Graze.HttpUrl  (serialize)
import Graze.Messages (LogCommand (..))


newtype Config = Config {logFile :: FilePath}

newtype Chans = Chans {inbox :: TChan LogCommand}

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    C8.writeFile logFile ""
    loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopLogging -> return ()
        Get t i url -> do
            C8.appendFile logFile $
                C8.pack t <> " " <> C8.pack i <> " GET " <> serialize url <> "\n"
            loop
