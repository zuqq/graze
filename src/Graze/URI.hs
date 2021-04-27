{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graze.URI
    ( parseURIRelativeTo
    -- * Reexports
    , URI (..)
    , URIAuth (..)
    , parseURI
    )
    where

import Data.Aeson (ToJSON (..))
import Network.URI

instance ToJSON URI where
    toJSON = toJSON . show

-- | Parse a URI reference and interpret it relative to the given 'URI'.
--
-- Note that the path of the result is normalized.
parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo base = fmap (`relativeTo` base) . parseURIReference
