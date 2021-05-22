module Graze.SHA1 (hash) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Hash the UTF-8 representation of a 'String' and express it in hexadecimal.
--
-- ==== __Examples__
--
-- >>> hash "abc"
-- "a9993e364706816aba3e25717850c26c9cd0d89d"
-- >>> hash "☃"
-- "2686137311c038a99622242fdb662b88c221c08d"
hash :: String -> String
hash = Char8.unpack . Base16.encode . SHA1.hash . Text.encodeUtf8 . Text.pack
