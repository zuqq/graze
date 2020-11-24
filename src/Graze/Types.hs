{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graze.Types
    (
    -- * Messages
    -- | Messages that are passed between threads.
    -- $commands
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

import Graze.Url


{- $commands

    The @…Command@ types are essentially 'Maybe' wrappers around the actual
    commands in order to provide an ad-hoc implementation of closeable queues:
    a thread that reads a @Stop…@ command puts the command back into the queue
    and exits.
-}

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

-- | The result that a fetcher passes back to the crawler.
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
--
-- The last three queues are bounded in order to provide sufficient
-- backpressure, i.e., to prevent the threads that process the results from
-- being overwhelmed. If any of them is full, all fetchers block; this gives
-- the other threads a chance to catch up. On the other hand, 'fetcherQueue'
-- needs to be unbounded because consuming a value from 'resultQueue' causes
-- writes to 'fetcherQueue'—a deadlock ensues!
data Queues = Queues
    { fetcherQueue :: TQueue FetcherCommand  -- ^ Shared job queue.
    , writerQueue  :: TBQueue WriterCommand  -- ^ Writer inbox.
    , loggerQueue  :: TBQueue LoggerCommand  -- ^ Logger inbox.
    , resultQueue  :: TBQueue Result         -- ^ Crawler inbox.
    }
