{-# LANGUAGE OverloadedStrings #-}

-- | Exports the 'Node' type that 'Graze.Crawler.crawl' outputs.
module Graze.Node (Node (..)) where

import Data.Aeson
import Data.Foldable (toList)
import Data.Set (Set)

import Graze.URI

-- | A node in the crawl tree.
data Node
    = Node
        !URI        -- ^ URL of the parent node.
        !URI        -- ^ URL of this node.
        !(Set URI)  -- ^ Outgoing links.
    deriving (Eq, Ord, Show)

instance ToJSON Node where
    toJSON (Node parent location links) =
        object
            [ "parent"   .= parent
            , "location" .= location
            , "links"    .= toList links
            ]

    toEncoding (Node parent location links) =
        pairs
            (   "parent"   .= parent
            <>  "location" .= location
            <>  "links"    .= toList links
            )
