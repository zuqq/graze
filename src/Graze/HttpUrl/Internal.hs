{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl.Internal
    ( HttpUrl (..)
    , hash
    , serialize
    ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8 (unpack)
import           Data.Hashable         (Hashable (hashWithSalt))

import qualified Crypto.Hash.SHA1       as SHA1   (hash)
import qualified Data.ByteString.Base16 as Base16 (encode)


data HttpUrl = HttpUrl
    { huScheme :: !B.ByteString
    , huDomain :: !B.ByteString
    , huPath   :: !B.ByteString
    }

instance Eq HttpUrl where
    x == y = (huDomain x, huPath x) == (huDomain y, huPath y)

instance Hashable HttpUrl where
    hashWithSalt salt x = hashWithSalt salt (huDomain x <> huPath x)

serialize :: HttpUrl -> B.ByteString
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

-- | Map an 'HttpUrl' to its base16-encoded SHA-1 digest.
hash :: HttpUrl -> String
hash = C8.unpack . Base16.encode . SHA1.hash . serialize
