{-# LANGUAGE LambdaCase #-}

module Graze.Writer
    ( write
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import qualified Data.ByteString              as B (appendFile, writeFile)
import qualified Data.Text.Encoding           as T (encodeUtf8)
import           Debug.Trace                  (traceIO)
import           System.FilePath              ((</>))

import Graze.HttpUrl  (hash)
import Graze.Messages (Instruction (..), Record (..))
import Graze.Records  (toText)


write
    :: FilePath           -- ^ Download folder.
    -> FilePath           -- ^ Page record file.
    -> TChan Instruction
    -> IO ()
write folder records outChan = loop
  where
    loop = atomically (readTChan outChan) >>= \case
        Stop      -> traceIO "Done"
        Write rec -> do
            B.appendFile records (T.encodeUtf8 (toText rec))
            B.writeFile (folder </> hash (rUrl rec)) (rContent rec)
            loop
