{-# LANGUAGE ViewPatterns #-}

module Graze.Links
    ( parseLinks
    )
    where

import Data.Either (rights)
import Data.Int (Int64)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text

import Graze.Links.Parser
import Graze.Url

longerThan :: Lazy.ByteString -> Int64 -> Bool
longerThan bs n = not . Lazy.null . Lazy.drop n $ bs

(!) :: Lazy.ByteString -> Int64 -> Word8
(!) = Lazy.index

isSpace :: Word8 -> Bool
isSpace w = case toEnum . fromIntegral $ w of
    '\t' -> True
    '\n' -> True
    '\f' -> True
    '\r' -> True
    ' '  -> True
    _    -> False

-- | Captures the content of all anchor start tags with at least one attribute.
--
-- This operates directly on the 'Lazy.ByteString' because that is lazier than
-- feeding the input into a parser. Note that it's okay to search for ASCII
-- characters directly because we're assuming UTF-8.
--
-- NB. Unfortunately the list of links in "Graze.Fetcher" still needs to be
-- materialized because it's used in two places.
lexLinks :: Lazy.ByteString -> [Lazy.ByteString]
lexLinks (Lazy.drop 1 . Lazy.dropWhile (/= 60) -> bs)
    | not $ bs `longerThan` 2          = []
    | bs ! 0 == 97 && isSpace (bs ! 1) = x : lexLinks bs'
    | otherwise                        = lexLinks bs
  where
    (x, bs') = Lazy.span (/= 62) bs

-- | @parseLinks base bs@ is a list of the links in the HTML document @bs@, with
-- @base@ serving as the base URL for relative links.
parseLinks :: Url -> Lazy.ByteString -> [Url]
parseLinks base = rights
    . fmap (parseLink base)
    . rights
    . fmap (Text.decodeUtf8' . Lazy.toStrict)
    . lexLinks
