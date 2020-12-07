{-# LANGUAGE LambdaCase #-}

module Graze.Logger
    ( runLogger
    )
    where

import Control.Concurrent.STM (STM, atomically)
import qualified Data.Text as T (Text, unpack)
import System.IO (hPutStrLn, stderr)

-- | Logs the URLs of downloaded pages to 'stderr'.
runLogger
    :: STM (Maybe T.Text)  -- ^ Receive a 'T.Text' to log.
    -> IO ()
runLogger recv = loop
  where
    loop = atomically recv >>= \case
        Nothing      -> pure ()
        Just message -> do
            hPutStrLn stderr . T.unpack $ message
            loop
