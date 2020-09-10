{-# LANGUAGE RecordWildCards #-}

module Graze.HttpUrl.Internal
    ( HttpUrl (..)
    , hashUrl
    , serializeUrl
    ) where

import qualified Data.ByteString.Base16 as Base16 (encode)
import qualified Data.ByteString.Char8  as BC (unpack)
import           Data.Hashable          (Hashable (hashWithSalt))
import qualified Data.Text              as T (Text)
import qualified Data.Text.Encoding     as T (encodeUtf8)

import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import           Data.Aeson       (ToJSON (..))

-- $setup
-- >>> :set -XOverloadedStrings


data HttpUrl = HttpUrl
    { scheme :: !T.Text
    , domain :: !T.Text
    , path   :: !T.Text
    }
    deriving (Read, Show)

-- | Ignores 'scheme'.
instance Eq HttpUrl where
    x == y = (domain x, path x) == (domain y, path y)

-- | Ignores 'scheme'.
instance Hashable HttpUrl where
    hashWithSalt salt x = hashWithSalt salt (domain x <> path x)

-- | Serialize the given 'HttpUrl', by concatenating its constituents.
--
-- ==== __Examples__
--
-- >>> serializeUrl $ HttpUrl "http:" "//www.example.com" "/"
-- "http://www.example.com/"
serializeUrl :: HttpUrl -> T.Text
serializeUrl HttpUrl {..} = scheme <> domain <> path

instance ToJSON HttpUrl where
    toJSON     = toJSON . serializeUrl
    toEncoding = toEncoding . serializeUrl

-- | Map an 'HttpUrl' to the Base16-encoded SHA-1 digest of its serialization.
--
-- ==== __Examples__
-- >>> hashUrl $ HttpUrl "http:" "//www.example.com" "/"
-- "89e6a0649e06d83370cdf2cbfb05f363934a8d0c"
hashUrl :: HttpUrl -> String
hashUrl = BC.unpack . Base16.encode . SHA1.hash . T.encodeUtf8 . serializeUrl
