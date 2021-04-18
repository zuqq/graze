{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graze.URI (URI (..), parseURI, parseURIRelativeTo) where

import Data.Aeson (ToJSON (..))
import Network.URI

instance ToJSON URI where
    toJSON = toJSON . show

parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo base = fmap (`relativeTo` base) . parseURIReference
