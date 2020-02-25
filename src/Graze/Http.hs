{-# LANGUAGE OverloadedStrings #-}

module Graze.Http (HttpUrl(..), folder, reqPage) where

import           Control.Monad              ((<=<))
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.Text.Lazy             as TL (dropWhileEnd, Text, unpack)

import Network.HTTP.Conduit (parseUrlThrow, redirectCount)
import Network.HTTP.Simple  (getResponseBody, httpLBS)

-- $setup
-- >>> :set -XOverloadedStrings


data HttpUrl = HttpUrl
    { scheme :: TL.Text
    , domain :: TL.Text
    , path   :: TL.Text
    }

-- ==== __Examples__
--
-- >>> x = HttpUrl "http" "x" "/y"
-- >>> y = HttpUrl "https" "x" "/y"
-- >>> x == y
-- True
instance Eq HttpUrl where
    x == y = (domain x, path x) == (domain y, path y)

-- ==== __Examples__
--
-- >>> x = HttpUrl "https" "x" "/"
-- >>> y = HttpUrl "http" "x" "/y"
-- >>> x < y
-- True
instance Ord HttpUrl where
    x <= y = (domain x, path x) <= (domain y, path y)

-- ==== __Examples__
--
-- >>> HttpUrl "http" "x" "/y"
-- http://x/y
instance Show HttpUrl where
    show url = TL.unpack $ scheme url <> "://" <> domain url <> path url

-- | Map an 'HttpUrl' to the folder components of its path.
--
-- ==== __Examples__
--
-- >>> folder $ HttpUrl "http" "x" "/y/"
-- "/y/"
-- >>> folder $ HttpUrl "http" "x" "/"
-- "/"
-- >>> folder $ HttpUrl "http" "x" "/y"
-- "/"
-- >>> folder $ HttpUrl "http" "x" "/y/z"
-- "/y/"
folder :: HttpUrl -> TL.Text
folder = TL.dropWhileEnd (/= '/') . path

reqPage :: HttpUrl -> IO BL.ByteString
reqPage = fmap getResponseBody
    . httpLBS
    <=< fmap noFollow
    . parseUrlThrow
    . show
  where
    noFollow r = r { redirectCount = 0 }
