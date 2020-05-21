module Graze.Messages
    ( FetchCommand (..)
    , FetchResult (..)
    , LogCommand (..)
    , Job (..)
    , Record (..)
    , WriteCommand (..)
    ) where

import qualified Data.ByteString as B (ByteString)

import Graze.HttpUrl (HttpUrl)


data Job = Job
    { jDepth  :: !Int      -- ^ Remaining depth of the search.
    , jOrigin :: !HttpUrl
    , jUrl    :: !HttpUrl
    }

data FetchCommand = StopFetching | Fetch !Job

data Record = Record
    { rJob   :: !Job
    , rLinks :: ![HttpUrl]
    , rBody  :: !B.ByteString
    }

data FetchResult = Failure | Success !Record

data WriteCommand = StopWriting | Write !Record

data LogCommand = StopLogging | Get !String !String !HttpUrl
