{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Messages that are passed between threads.
module Graze.Types
    ( Job (..)
    , Record (..)
    , Result (..)
    , Write (..)
    )
    where

import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as Lazy

import Graze.Url

-- | A page to visit.
data Job = Job
    { origin :: !Url
    , url    :: !Url
    , depth  :: !Int  -- ^ Remaining depth of the search.
    }

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
data Write = Write !Record !Lazy.ByteString
