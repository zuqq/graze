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
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy

import Graze.URI

-- | A page to visit.
data Job = Job
    { origin :: !URI
    , url    :: !URI
    , depth  :: !Int  -- ^ Remaining depth of the search.
    }

-- | The result that a fetcher passes back to the crawler.
data Report
    = Failure
    | Success !Job !(Set URI) !Lazy.ByteString

-- | Metadata for a visited page.
data Record = Record
    { origin :: !URI
    , url    :: !URI
    , links  :: ![URI]
    }
    deriving Generic

instance ToJSON Record where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Result = Result !Record !Lazy.ByteString
