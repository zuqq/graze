{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher (fetch) where

import Control.Exception (try)

import Graze.HTML
import Graze.Http
import Graze.Types (Job (Job), Report (..))

-- | Fetcher thread.
--
-- A unit of work for a fetcher consists of downloading and parsing a page;
-- it then passes the result back to the crawler.
fetch
    :: IO (Maybe Job)     -- ^ Receive a 'Job'.
    -> (Report -> IO ())  -- ^ Send a 'Report' to the crawler.
    -> IO ()
fetch receive send = loop
  where
    loop =
        receive >>= \case
            Nothing -> pure ()
            Just job@(Job _ uri _) -> do
                try (get ("text" // "html") "graze" uri) >>= \case
                    Left (_ :: HttpException) -> send Failure
                    Right (Just s) -> send (Success job (parseLinks uri s))
                    Right _ -> send (Success job mempty)
                loop
