{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graze.Messages
    ( Chans (..)
    , FetcherCommand (..)
    , Job (..)
    , LoggerCommand (..)
    , Record (..)
    , Result (..)
    , WriterCommand (..)
    ) where

import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.ByteString.Lazy         as BL (ByteString)
import qualified Data.Text                    as T (Text)
import           GHC.Generics                 (Generic)

import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)

import Graze.HttpUrl (HttpUrl)


data Job = Job
    { origin :: !HttpUrl
    , url    :: !HttpUrl
    , depth  :: !Int      -- ^ Remaining depth of the search.
    }

data FetcherCommand
    = StopFetching
    | Fetch !Job

data Result
    = Failure
    | Success !Job ![HttpUrl] !BL.ByteString

data Record = Record
    { origin :: !HttpUrl
    , url    :: !HttpUrl
    , links  :: ![HttpUrl]
    }
    deriving Generic

instance ToJSON Record where
    toEncoding = genericToEncoding defaultOptions

data WriterCommand
    = StopWriting
    | Write !Record !BL.ByteString

data LoggerCommand
    = StopLogging
    | Log !T.Text

data Chans = Chans
    { fetcherChan :: TChan FetcherCommand
    , resultChan  :: TChan Result
    , writerChan  :: TChan WriterCommand
    , loggerChan  :: TChan LoggerCommand
    }
