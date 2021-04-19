{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher (fetch) where

import Control.Concurrent.STM
import Control.Exception (try)
import Data.Text (Text)

import Graze.HTML
import Graze.Http
import Graze.Types

-- | Fetcher thread.
--
-- A unit of work for a fetcher consists of downloading and parsing a page;
-- it then passes the result back to the crawler.
fetch
    :: STM (Maybe Job)     -- ^ Receive a 'Job'.
    -> (Report -> STM ())  -- ^ Send a 'Report' to the crawler.
    -> IO ()
fetch recvJob sendReport = loop
  where
    loop = do
        mjob <- atomically recvJob
        case mjob of
            Nothing           -> pure ()
            Just job@Job {..} -> do
                response :: Either HttpException (Maybe Text) <-
                    try (get ("text" // "html") "graze" uri)
                case response of
                    Left _         -> atomically . sendReport $ Failure
                    Right (Just s) -> atomically . sendReport $
                        Success job (parseLinks uri s)
                    Right _        -> atomically . sendReport $
                        Success job mempty
                loop
