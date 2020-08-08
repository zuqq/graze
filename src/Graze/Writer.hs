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
import qualified Data.ByteString.Lazy         as BL (writeFile)
import qualified Data.Text.Lazy.Encoding      as TL (encodeUtf8)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((<.>), (</>))


import Graze.HttpUrl  (hash, serialize)
import Graze.Messages (Job (..), Record (..), WriteCommand (..))
import Graze.SExpr    (SExpr (..), toLazyText)


newtype Config = Config {folder :: FilePath}

newtype Chans = Chans {inbox :: TChan WriteCommand}

toSExpr :: Record -> SExpr
toSExpr r = Node
    [ Node [Leaf "origin", origin]
    , Node [Leaf "url"   , url   ]
    , Node [Leaf "links" , links ]
    ]
  where
    origin = Leaf . serialize . jOrigin . rJob $ r
    url    = Leaf . serialize . jUrl . rJob $ r
    links  = Node . fmap (Leaf . serialize) . rLinks $ r

run :: Config -> Chans -> IO ()
run Config {..} Chans {..} = do
    createDirectoryIfMissing True folder
    loop
  where
    loop = atomically (readTChan inbox) >>= \case
        StopWriting  -> return ()
        Write record -> do
            let name = hash . jUrl . rJob $ record
            BL.writeFile (folder </> name) (rBody record)
            BL.writeFile
                (folder </> name <.> "sexp")
                (TL.encodeUtf8 . toLazyText . toSExpr $ record)
            loop
