{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.Http (HttpUrl(..), folder, reqPage) where

import           Control.Monad         ((<=<))
import qualified Data.ByteString.Char8 as B (ByteString)
import qualified Data.Text             as T (dropWhileEnd, Text, unpack)

import Network.HTTP.Conduit (parseUrlThrow, redirectCount)
import Network.HTTP.Simple  (getResponseBody, httpBS)

-- $setup
-- >>> :set -XOverloadedStrings


data HttpUrl = HttpUrl
    { scheme :: !T.Text
    , domain :: !T.Text
    , path   :: !T.Text
    }

-- >>> x = HttpUrl "http" "x" "/y"
-- >>> y = HttpUrl "https" "x" "/y"
-- >>> x == y
-- True
instance Eq HttpUrl where
    x == y = (domain x, path x) == (domain y, path y)

-- >>> x = HttpUrl "https" "x" "/"
-- >>> y = HttpUrl "http" "x" "/y"
-- >>> x < y
-- True
instance Ord HttpUrl where
    x <= y = (domain x, path x) <= (domain y, path y)

-- >>> HttpUrl "http" "x" "/y"
-- http://x/y
instance Show HttpUrl where
    show HttpUrl {..} = T.unpack $ scheme <> "://" <> domain <> path

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
folder :: HttpUrl -> T.Text
folder = T.dropWhileEnd (/= '/') . path

reqPage :: HttpUrl -> IO B.ByteString
reqPage = fmap getResponseBody
    . httpBS
    <=< fmap noFollow
    . parseUrlThrow
    . show
  where
    noFollow r = r { redirectCount = 0 }
