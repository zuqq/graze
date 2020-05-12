module Graze.Http (reqPage) where

import           Control.Monad   ((<=<))
import qualified Data.ByteString as B (ByteString)
import qualified Data.Text       as T (unpack)

import Network.HTTP.Conduit (parseUrlThrow, redirectCount)
import Network.HTTP.Simple  (getResponseBody, httpBS)

import Graze.HttpUrl (HttpUrl, serialize)


reqPage :: HttpUrl -> IO B.ByteString
reqPage = fmap getResponseBody
    . httpBS
    <=< fmap noFollow
    . parseUrlThrow
    . T.unpack
    . serialize
  where
    noFollow r = r { redirectCount = 0 }
