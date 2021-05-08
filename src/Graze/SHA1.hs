module Graze.SHA1 (hash) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8

hash :: String -> String
hash = Char8.unpack . Base16.encode . SHA1.hash . Char8.pack
