{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Graze.Writer
    ( runWriter
    )
    where

import Control.Concurrent.STM (STM, atomically)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

import Graze.Types
import Graze.Url

-- | Writes 'Record's to the folder at the given 'FilePath'.
runWriter
    :: FilePath           -- ^ Folder to download to.
    -> STM (Maybe Write)  -- ^ Receive a 'Write'.
    -> IO ()
runWriter folder recv = do
    createDirectoryIfMissing True (folder </> "records")
    loop
  where
    loop = atomically recv >>= \case
        Nothing                  -> pure ()
        Just (Write record body) -> do
            let Record _ (hashUrl -> name) _ = record
            BL.writeFile (folder </> "records" </> name <.> "json") (encode record)
            BL.writeFile (folder </> name) body
            loop
