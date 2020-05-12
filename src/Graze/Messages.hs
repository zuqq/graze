module Graze.Messages
    ( Done (..)
    , FetchResponse (..)
    , Job (..)
    , PageRecord (..)
    , Result (..)
    ) where

import qualified Data.ByteString as B (ByteString)

import Graze.HttpUrl (HttpUrl)


data Job = Job
    { jDepth  :: !Int      -- ^ Remaining depth of the search.
    , jParent :: !HttpUrl
    , jUrl    :: !HttpUrl
    }

data Result = Fail | Success !B.ByteString

data FetchResponse = FetchResponse
    { frJob    :: !Job
    , frResult :: !Result
    }

data Done = Done

data PageRecord = PageRecord
    { prParent   :: !HttpUrl
    , prUrl      :: !HttpUrl
    , prChildren :: ![HttpUrl]
    , prContent  :: !B.ByteString
    }
