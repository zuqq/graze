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
import qualified Data.ByteString              as B (writeFile)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((<.>), (</>))


import Graze.HttpUrl  (hash, serialize)
import Graze.Messages
import Graze.SExpr    (SExpr (..), toByteString)


data Config = Config
    { folder  :: !FilePath  -- ^ Download folder.
    }

data Chans = Chans
    { inbox :: !(TChan WriteCommand)
    }

toSExpr :: Record -> SExpr
toSExpr record = Node
    [ Node [Leaf "origin", Leaf origin]
    , Node [Leaf "url"   , Leaf url   ]
    , Node [Leaf "links" , Node links ]
    ]
  where
    job    = rJob record
    origin = serialize (jOrigin job)
    url    = serialize (jUrl job)
    links  = Leaf . serialize <$> rLinks record

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    createDirectoryIfMissing True folder
    loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopWriting  -> return ()
        Write record -> do
            let name = hash . jUrl . rJob $ record
            B.writeFile (folder </> name) (rBody record)
            B.writeFile (folder </> name <.> "sexp") (toByteString . toSExpr $ record)
            loop
