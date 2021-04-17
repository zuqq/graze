{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graze.URI (URI (..), parseRelURI, parseURI) where

import Data.Aeson (ToJSON (..))
import Network.URI

instance ToJSON URI where
    toJSON = toJSON . show

parseRelURI :: URI -> String -> Maybe URI
parseRelURI base s = (`relativeTo` base) <$> parseURIReference s
