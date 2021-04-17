{-# LANGUAGE ViewPatterns #-}

module Graze.Links (parseLinks) where

import Data.Either (rights)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text

import Graze.Links.Parser
import Graze.URI

lengthSmallerThan :: Int64 -> Lazy.ByteString -> Bool
lengthSmallerThan n = Lazy.null . Lazy.drop n

(!) :: Lazy.ByteString -> Int64 -> Word8
(!) = Lazy.index

isSpace :: Word8 -> Bool
isSpace w = case w of
    9  -> True
    10 -> True
    12 -> True
    13 -> True
    32 -> True
    _  -> False

-- | Captures the content of all anchor start tags with at least one attribute.
--
-- This operates directly on the 'Lazy.ByteString' because that is lazier than
-- feeding the input into a parser. Note that it's okay to search for ASCII
-- characters directly because we're assuming UTF-8.
lexLinks :: Lazy.ByteString -> [Lazy.ByteString]
lexLinks (Lazy.drop 1 . Lazy.dropWhile (/= 60) -> bs)
    | lengthSmallerThan 2 bs           = []
    | bs ! 0 == 97 && isSpace (bs ! 1) = x : lexLinks bs'
    | otherwise                        = lexLinks bs
  where
    (x, bs') = Lazy.span (/= 62) bs

-- | @parseLinks base bs@ is a list of the links in the HTML document @bs@, with
-- @base@ serving as the base URL for relative links.
parseLinks :: URI -> Lazy.ByteString -> [URI]
parseLinks base
    = mapMaybe (parseLink base)
    . rights
    . fmap (Text.decodeUtf8' . Lazy.toStrict)
    . lexLinks
