{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl
    ( HttpUrl (huDomain, huPath, huScheme)
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


serialize :: HttpUrl -> B.ByteString
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

-- | Map an 'HttpUrl' to its base16-encoded SHA-1 digest.
hash :: HttpUrl -> String
hash = C8.unpack . Base16.encode . SHA1.hash . serialize
