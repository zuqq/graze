{-# LANGUAGE RecordWildCards #-}

module Graze.Url.Types
    ( Url (..)
    , hashUrl
    , serializeUrl
    )
    where

import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Base16 as Base16 (encode)
import qualified Data.ByteString.Char8 as BC (unpack)
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T (encodeUtf8)

-- $setup
-- >>> :set -XOverloadedStrings


-- | A simplified URL type of exactly the granularity that we need.
--
-- Namely, we need access to the scheme in order to resolve protocol-relative
-- HTML links, and access to the domain and path in order to apply the
-- robots.txt file.
data Url = Url
    { scheme :: !T.Text
    , domain :: !T.Text
    , path   :: !T.Text
    }
    deriving Show

-- | Ignores 'scheme'.
instance Eq Url where
    x == y = (domain x, path x) == (domain y, path y)

-- | Ignores 'scheme'.
instance Hashable Url where
    hashWithSalt salt x = hashWithSalt salt (domain x <> path x)

-- | Serialize the given 'Url', by concatenating its constituents.
--
-- ==== __Examples__
--
-- >>> serializeUrl $ Url "http:" "//www.example.com" "/"
-- "http://www.example.com/"
serializeUrl :: Url -> T.Text
serializeUrl Url {..} = scheme <> domain <> path

instance ToJSON Url where
    toJSON     = toJSON . serializeUrl
    toEncoding = toEncoding . serializeUrl

-- | Map an 'Url' to the Base16-encoded SHA-1 digest of its serialization.
--
-- ==== __Examples__
-- >>> hashUrl $ Url "http:" "//www.example.com" "/"
-- "89e6a0649e06d83370cdf2cbfb05f363934a8d0c"
hashUrl :: Url -> String
hashUrl = BC.unpack . Base16.encode . SHA1.hash . T.encodeUtf8 . serializeUrl
