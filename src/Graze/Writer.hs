{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer
    ( Chans (Chans)
    , Config (Config)
    , run
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import qualified Data.ByteString              as B (appendFile, writeFile)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))

import Graze.HttpUrl  (hash, serialize)
import Graze.Messages
import Graze.SExpr    (SExpr (..), toByteString)


data Config = Config
    { folder  :: !FilePath  -- ^ Download folder.
    , records :: !FilePath  -- ^ Page record file.
    }

data Chans = Chans
    { inbox :: !(TChan WriteCommand)
    }

toSExpr :: Record -> SExpr
toSExpr record = Node
    [ Node [Leaf "origin" , Leaf origin]
    , Node [Leaf "url" , Leaf url]
    , Node [Leaf "links" , Node links]
    ]
  where
    job    = rJob record
    origin = serialize (jOrigin job)
    url    = serialize (jUrl job)
    links  = Leaf . serialize <$> rLinks record

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    createDirectoryIfMissing True folder
    B.writeFile records ""
    loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopWriting  -> return ()
        Write record -> do
            let url  = jUrl (rJob record)
                body = rBody record
            B.writeFile (folder </> hash url) body
            B.appendFile records (toByteString (toSExpr record) <> "\n")
            loop
