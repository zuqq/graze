{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graze.Types
    (
    -- * Messages
    -- | Messages that are passed between threads.
      FetcherCommand (..)
    , Job (..)
    , LoggerCommand (..)
    , Record (..)
    , Result (..)
    , WriterCommand (..)
    -- * Queues
    , Queues (..)
    ) where

import           Control.Concurrent.STM.TQueue  (TQueue)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Data.ByteString.Lazy           as BL (ByteString)
import qualified Data.Text                      as T (Text)
import           GHC.Generics                   (Generic)

import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)

import Graze.Url (Url)


-- | A page to visit.
data Job = Job
    { origin :: !Url
    , url    :: !Url
    , depth  :: !Int  -- ^ Remaining depth of the search.
    }

-- | Instructions for a fetcher.
data FetcherCommand
    = StopFetching
    | Fetch !Job

-- | The result of a page visit that a fetcher passes back to the main thread.
data Result
    = Failure
    | Success !Job ![Url]

-- | Metadata for a visited page.
data Record = Record
    { origin :: !Url
    , url    :: !Url
    , links  :: ![Url]
    }
    deriving Generic

instance ToJSON Record where
    toEncoding = genericToEncoding defaultOptions

-- | Instructions for the writer.
data WriterCommand
    = StopWriting
    | Write !Record !BL.ByteString

-- | Instructions for the logger.
data LoggerCommand
    = StopLogging
    | Log !T.Text

-- | Queues that the different threads use to communicate.
data Queues = Queues
    { fetcherQueue :: TQueue FetcherCommand
    , writerQueue  :: TBQueue WriterCommand
    , loggerQueue  :: TBQueue LoggerCommand
    , resultQueue  :: TBQueue Result
    }
