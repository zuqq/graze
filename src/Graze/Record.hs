{-# LANGUAGE OverloadedStrings #-}

module Graze.Record (Record (..)) where

import Data.Aeson
import Data.Foldable (toList)
import Data.Set (Set)

import Graze.URI

-- | Metadata for a visited page.
data Record = Record
    !URI        -- ^ URI of the parent node.
    !URI        -- ^ URI of this node.
    !(Set URI)  -- ^ Outgoing links.
    deriving (Eq, Ord, Show)

instance ToJSON Record where
    toJSON (Record parent uri links) =
        object
            [ "parent" .= parent
            , "uri"    .= uri
            , "links"  .= toList links
            ]

    toEncoding (Record parent uri links) =
        pairs
            (   "parent" .= parent
            <>  "uri"    .= uri
            <>  "links"  .= toList links
            )
