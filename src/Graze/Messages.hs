module Graze.Messages
    ( Instruction (..)
    , Job (..)
    , Record (..)
    , Report (..)
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

data Report = Report !Job !Result

data Record = Record
    { rParent   :: !HttpUrl
    , rUrl      :: !HttpUrl
    , rChildren :: ![HttpUrl]
    , rContent  :: !B.ByteString
    }

data Instruction = Stop | Write !Record
