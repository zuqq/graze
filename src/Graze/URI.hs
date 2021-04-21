{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graze.URI
    ( parseURIRelativeTo
    -- * Reexports
    , URI (..)
    , parseURI
    )
    where

import Data.Aeson (ToJSON (..))
import Network.URI

instance ToJSON URI where
    toJSON = toJSON . show

-- | Parse a URI reference and interpret it relative to the given 'URI'.
parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo base = fmap (`relativeTo` base) . parseURIReference
