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
import           Data.Time.Format

import Graze.Messages (LogCommand (..), Level (..), Message (..))


newtype Config = Config {logFile :: FilePath}

newtype Chans = Chans {inbox :: TChan LogCommand}

format :: FormatTime t => t -> C8.ByteString
format = C8.pack . formatTime defaultTimeLocale "%H:%M:%S,%3q"

tag :: Level -> C8.ByteString
tag Debug   = "DEBUG:"
tag Info    = "INFO:"
tag Warning = "WARNING:"
tag Error   = "ERROR:"

toByteString :: Message -> C8.ByteString
toByteString (Message time level body) =
    C8.intercalate " " [format time, tag level, body] <> "\n"

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    C8.writeFile logFile ""
    loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopLogging -> return ()
        Log message -> do
            C8.appendFile logFile $
                toByteString message
            loop
