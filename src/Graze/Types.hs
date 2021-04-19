{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Messages that are passed between threads.
module Graze.Types
    ( Job (..)
    , Record (..)
    , Report (..)
    )
    where

import Data.Aeson
import Data.Foldable (toList)
import Data.Set (Set)

import Graze.URI

-- | A page to visit.
data Job = Job
    { origin :: !URI
    , uri    :: !URI
    , depth  :: !Int  -- ^ Remaining depth of the search.
    }
    deriving (Eq, Ord)

-- | The result that a fetcher passes back to the crawler.
data Report = Failure | Success !Job !(Set URI)

-- | Metadata for a visited page.
data Record = Record
    { origin :: !URI
    , uri    :: !URI
    , links  :: !(Set URI)
    }
    deriving (Eq, Ord, Show)

instance ToJSON Record where
    toJSON Record {..}     =
        object
            [ "origin" .= origin
            , "uri"    .= uri
            , "links"  .= toList links
            ]
    toEncoding Record {..} =
        pairs
            (   "origin" .= origin
            <>  "uri"    .= uri
            <>  "links"  .= toList links
            )
