{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl
    ( HttpUrl (..)
    , hash
    , parse
    , parseRelTo
    , serialize
    ) where

import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as C8 (unpack)

import qualified Crypto.Hash.SHA1       as SHA1   (hash)
import qualified Data.ByteString.Base16 as Base16 (encode)

import Graze.HttpUrl.Internal (HttpUrl (..))
import Graze.HttpUrl.Parser   (parse, parseRelTo)

-- $setup
-- >>> :set -XOverloadedStrings


-- | Serialize the given 'HttpUrl', by concatenating its constituents.
--
-- ==== __Examples__
--
-- >>> serialize $ HttpUrl "http:" "//www.example.com" "/"
-- "http://www.example.com/"
serialize :: HttpUrl -> B.ByteString
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

-- | Map an 'HttpUrl' to the base16-encoded SHA-1 digest of its serialization.
--
-- ==== __Examples__
-- >>> hash $ HttpUrl "http:" "//www.example.com" "/"
-- "89e6a0649e06d83370cdf2cbfb05f363934a8d0c"
hash :: HttpUrl -> String
hash = C8.unpack . Base16.encode . SHA1.hash . serialize
