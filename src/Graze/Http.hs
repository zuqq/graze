module Graze.Http (reqPage) where

import           Control.Monad         ((<=<))
import qualified Data.ByteString.Char8 as B (ByteString)

import Network.HTTP.Conduit (parseUrlThrow, redirectCount)
import Network.HTTP.Simple  (getResponseBody, httpBS)

import Graze.HttpUrl (HttpUrl)


reqPage :: HttpUrl -> IO B.ByteString
reqPage = fmap getResponseBody
    . httpBS
    <=< fmap noFollow
    . parseUrlThrow
    . show
  where
    noFollow r = r { redirectCount = 0 }
