{-# LANGUAGE ViewPatterns #-}

module Graze.Links
    ( parseLinks
    ) where

import qualified Data.ByteString.Lazy as LB
import           Data.Either          (rights)
import           Data.Int             (Int64)
import qualified Data.Text.Encoding   as T (decodeUtf8')
import           Data.Word            (Word8)

import Graze.Links.Parser
import Graze.Url


longerThan :: LB.ByteString -> Int64 -> Bool
longerThan bs n = not . LB.null . LB.drop n $ bs

(!) :: LB.ByteString -> Int64 -> Word8
(!) = LB.index

isSpace :: Word8 -> Bool
isSpace (toEnum . fromIntegral -> c) = case c of
    '\t' -> True
    '\n' -> True
    '\f' -> True
    '\r' -> True
    ' '  -> True
    _    -> False

{-
    Captures the content of all anchor start tags with at least one attribute.

    This operates directly on the 'LB.ByteString' because that is lazier than
    feeding the input into a parser. Note that it's okay to search for ASCII
    characters directly because we're assuming UTF-8.

    NB. Unfortunately the list of links in "Graze.Fetcher" still needs to be
    materialized because it's used in two places.
-}
lexLinks :: LB.ByteString -> [LB.ByteString]
lexLinks (LB.drop 1 . LB.dropWhile (/= 60) -> bs)
    | not $ bs `longerThan` 2          = []
    | bs ! 0 == 97 && isSpace (bs ! 1) = x : lexLinks bs'
    | otherwise                        = lexLinks bs
  where
    (x, bs') = LB.span (/= 62) bs

-- | @parseLinks base bs@ is a list of the links in the HTML document @bs@, with
-- @base@ serving as the base URL for relative links.
parseLinks :: Url -> LB.ByteString -> [Url]
parseLinks base = rights
    . fmap (parseLink base)
    . rights
    . fmap (T.decodeUtf8' . LB.toStrict)
    . lexLinks
