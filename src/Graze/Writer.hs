{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Writer (evalWriter, write, WriterState(..)) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (evalStateT, get, modify, StateT)
import qualified Data.ByteString.Lazy.Char8 as BL
import Debug.Trace (traceIO)
import System.FilePath ((</>))

import Graze.Messages (Done, PageRecord(..))


data WriterState = WriterState
    { wsCounter  :: !Int
    , wsFolder   :: !FilePath
    , wsDatabase :: !FilePath
    }

type Writer a = StateT WriterState IO a

evalWriter :: Writer a -> WriterState -> IO a
evalWriter = evalStateT

encRecord :: Int -> PageRecord -> BL.ByteString
encRecord counter PageRecord {..} =
    show' counter  <> ","    <>
    show' prParent <> ","    <>
    show' prUrl    <> "\r\n"
  where
    show' :: Show a => a -> BL.ByteString
    show' = BL.pack . show

writeRecord :: PageRecord -> Writer ()
writeRecord record = do
    WriterState {..} <- get
    liftIO $ do
        BL.appendFile (wsFolder </> wsDatabase) (encRecord wsCounter record)
        BL.writeFile (wsFolder </> show wsCounter) (prContent record)

mapCounter :: (Int -> Int) -> Writer ()
mapCounter f = modify $ \s ->
    s { wsCounter = f (wsCounter s) }

write :: TChan (Either Done PageRecord) -> Writer ()
write outChan = loop
  where
    loop = (liftIO . atomically . readTChan) outChan >>= \case
        Left _       -> liftIO . traceIO $ "Done"
        Right record -> do
            writeRecord record
            mapCounter (+ 1)
            loop
