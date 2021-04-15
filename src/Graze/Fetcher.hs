{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graze.Fetcher (fetch) where

import Control.Concurrent.STM
import Control.Exception (try)

import Graze.Http
import Graze.Links
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
                eresponse :: Either HttpException Response <- try $ get url
                case eresponse of
                    Left _                  -> atomically . sendReport $ Failure
                    Right (contentType, bs) -> do
                        let links = case contentType of
                                TextHtml -> parseLinks url bs
                                _        -> []
                        atomically . sendReport $ Success job links bs
                loop
