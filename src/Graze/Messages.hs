{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Graze.Messages
    ( FetchCommand (..)
    , FetchResult (..)
    , Job (..)
    , LogCommand (..)
    , Record (..)
    , WriteCommand (..)
    ) where

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Text            as T (Text)
import           GHC.Generics         (Generic)

import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)

import Graze.HttpUrl (HttpUrl)


data Job = Job
    { origin :: !HttpUrl
    , url    :: !HttpUrl
    , depth  :: !Int      -- ^ Remaining depth of the search.
    }

data FetchCommand
    = StopFetching
    | Fetch !Job

data FetchResult
    = Failure
    | Success !Job ![HttpUrl] !BL.ByteString

data Record = Record
    { origin :: !HttpUrl
    , url    :: !HttpUrl
    , links  :: ![HttpUrl]
    }
    deriving Generic

instance ToJSON Record where
    toEncoding = genericToEncoding defaultOptions

data WriteCommand
    = StopWriting
    | Write !Record !BL.ByteString

data LogCommand
    = StopLogging
    | Log !T.Text
