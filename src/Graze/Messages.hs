{-# OPTIONS_GHC -Wall -Werror #-}

module Graze.Messages where

import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)

import Graze.Http (HttpUrl)


data Job = Job
    { jHops   :: !Int      -- ^ Remaining depth of the search.
    , jParent :: !HttpUrl
    , jUrl    :: !HttpUrl
    }

data Result = Fail | Success !BL.ByteString

data FetchResponse = FetchResponse
    { frJob    :: !Job
    , frResult :: !Result
    }

data Done = Done

data PageRecord = PageRecord
    { prParent  :: !HttpUrl
    , prUrl     :: !HttpUrl
    , prContent :: !BL.ByteString
    }
