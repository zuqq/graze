{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer (evalWriter, write, WriterState (..)) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TChan   (readTChan, TChan)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.State.Lazy (evalStateT, get, StateT)
import qualified Data.ByteString                as B
import qualified Data.Text.Encoding             as T (encodeUtf8)
import           Debug.Trace                    (traceIO)
import           System.FilePath                ((</>))

import Graze.HttpUrl  (hash)
import Graze.Messages (Done, PageRecord (..))
import Graze.Records  (toSExpr)


data WriterState = WriterState
    { wsFolder   :: !FilePath
    , wsDatabase :: !FilePath
    }

type Writer a = StateT WriterState IO a

evalWriter :: Writer a -> WriterState -> IO a
evalWriter = evalStateT

writeRecord :: PageRecord -> Writer ()
writeRecord record = do
    WriterState {..} <- get
    liftIO $ do
        B.appendFile (wsFolder </> wsDatabase) (toSExpr' record)
        B.writeFile (wsFolder </> hash (prUrl record)) (prContent record)
  where
    toSExpr' = T.encodeUtf8 . toSExpr

write :: TChan (Either Done PageRecord) -> Writer ()
write outChan = loop
  where
    loop = (liftIO . atomically . readTChan) outChan >>= \case
        Left _       -> liftIO . traceIO $ "Done"
        Right record -> writeRecord record >> loop
