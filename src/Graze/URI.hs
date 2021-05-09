{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A wrapper around "Network.URI".
module Graze.URI
    ( parseURIRelativeTo
    -- * Reexports
    , URI (..)
    , URIAuth (..)
    , parseURI
    )
    where

import Data.Aeson
import Network.URI

orFail :: MonadFail m => Maybe a -> String -> m a
orFail x e = maybe (fail e) pure x

instance FromJSON URI where
    parseJSON v = do
        s <- parseJSON v
        parseURI s `orFail` "Invalid URI."

instance ToJSON URI where
    toJSON = toJSON . show

-- | Parse a URI reference and interpret it relative to the given 'URI'.
--
-- Note that the path of the result is normalized.
parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo base = fmap (`relativeTo` base) . parseURIReference
