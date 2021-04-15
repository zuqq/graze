{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Messages that are passed between threads.
module Graze.Types
    ( Job (..)
    , Record (..)
    , Report (..)
    , Result (..)
    )
    where

import Data.Aeson (ToJSON (..))
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy

import Graze.Url

-- | A page to visit.
data Job = Job
    { origin :: !Url
    , url    :: !Url
    , depth  :: !Int  -- ^ Remaining depth of the search.
    }

-- | The result that a fetcher passes back to the crawler.
data Report
    = Failure
    | Success !Job ![Url] !Lazy.ByteString

-- | Metadata for a visited page.
data Record = Record
    { origin :: !Url
    , url    :: !Url
    , links  :: ![Url]
    }
    deriving Generic

instance ToJSON Record where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Result = Result !Record !Lazy.ByteString
