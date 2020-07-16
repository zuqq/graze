{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Logger
    ( Chans (Chans)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import qualified Data.ByteString.Char8        as C8
import           Data.Time.Format
import           System.IO                    (stderr)

import Graze.Messages (LogCommand (..), Level (..), Message (..))


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
    C8.unwords [format time, tag level, body]

run :: Chans -> IO ()
run Chans {..} = loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopLogging -> return ()
        Log message -> do
            C8.hPutStrLn stderr . toByteString $ message
            loop
