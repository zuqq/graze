{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exports the 'Node' type that 'Graze.Crawler.crawl' outputs.
module Graze.Node (Node (..)) where

import Data.Aeson
import Data.Set (Set)
import GHC.Generics (Generic)

import Graze.URI

-- | A node in the crawl tree.
data Node = Node
    { nodeParent   :: !URI        -- ^ URL of the parent node.
    , nodeLocation :: !URI        -- ^ URL of this node.
    , nodeLinks    :: !(Set URI)  -- ^ Outgoing links.
    }
    deriving (Eq, Generic, Ord, Show)

instance FromJSON Node

instance ToJSON Node where
    toEncoding = genericToEncoding defaultOptions
