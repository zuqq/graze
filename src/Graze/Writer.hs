{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Graze.Writer (evalWriter, write, WriterState(..)) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (evalStateT, gets, modify, StateT)
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
encRecord counter record =
    show' counter           <> ","    <>
    show' (prParent record) <> ","    <>
    show' (prUrl record)    <> "\r\n"
  where
    show' :: Show a => a -> BL.ByteString
    show' = BL.pack . show

addRecord :: PageRecord -> Writer ()
addRecord record = do
    counter  <- gets wsCounter
    folder   <- gets wsFolder
    database <- gets wsDatabase
    liftIO $ BL.appendFile (folder </> database) (encRecord counter record)

writeRecord :: PageRecord -> Writer ()
writeRecord record = do
    counter <- gets wsCounter
    folder  <- gets wsFolder
    liftIO $ BL.writeFile (folder </> show counter) (prContent record)

incrCounter :: Writer ()
incrCounter = modify $ \s ->
    s { wsCounter = wsCounter s + 1 }

write :: TChan (Either Done PageRecord) -> Writer ()
write outChan = loop
  where
    loop = (liftIO . atomically) (readTChan outChan) >>= \case
        Left _       -> liftIO . traceIO $ "Done"
        Right record -> do
            addRecord record
            writeRecord record
            incrCounter
            loop
