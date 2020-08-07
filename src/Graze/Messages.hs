module Graze.Messages
    ( FetchCommand (..)
    , FetchResult (..)
    , Job (..)
    , LogCommand (..)
    , Level (..)
    , Message (..)
    , Record (..)
    , WriteCommand (..)
    ) where

import qualified Data.ByteString      as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import           Data.Time.LocalTime  (ZonedTime)

import Graze.HttpUrl (HttpUrl)


data Job = Job
    { jDepth  :: !Int      -- ^ Remaining depth of the search.
    , jOrigin :: !HttpUrl
    , jUrl    :: !HttpUrl
    }

data FetchCommand
    = StopFetching
    | Fetch !Job

data Record = Record
    { rJob   :: !Job
    , rLinks :: ![HttpUrl]
    , rBody  :: !L.ByteString
    }

data FetchResult
    = Failure
    | Success !Record

data WriteCommand
    = StopWriting
    | Write !Record

data Level
    = Debug
    | Info
    | Warning
    | Error

data Message = Message
    !ZonedTime
    !Level
    !B.ByteString

data LogCommand
    = StopLogging
    | Log !Message
