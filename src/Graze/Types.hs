{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Messages that are passed between threads.
module Graze.Types
    ( Job (..)
    , Record (..)
    , Report (..)
    )
    where

import Data.Aeson
import Data.Set (Set)
import GHC.Generics (Generic)

import Graze.URI

-- | A page to visit.
data Job = Job
    { origin :: !URI
    , uri    :: !URI
    , depth  :: !Int  -- ^ Remaining depth of the search.
    }

-- | The result that a fetcher passes back to the crawler.
data Report = Failure | Success !Job !(Set URI)

-- | Metadata for a visited page.
data Record = Record
    { origin :: !URI
    , uri    :: !URI
    , links  :: ![URI]
    }
    deriving Generic

instance ToJSON Record where
    toEncoding = genericToEncoding defaultOptions
