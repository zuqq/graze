{-# LANGUAGE OverloadedStrings #-}

module Graze.Messages
    ( FetchCommand (..)
    , FetchResult (..)
    , Job (..)
    , LogCommand (..)
    , Record (..)
    , WriteCommand (..)
    ) where

import           Data.Aeson           ((.=), ToJSON (..), object, pairs)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Text            as T (Text)

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
    , rBody  :: !BL.ByteString
    }

instance ToJSON Record where
    toJSON (Record (Job _ origin url) links _)     =
        object ["origin" .= origin, "url" .= url, "links" .= links]
    toEncoding (Record (Job _ origin url) links _) =
        pairs $ "origin" .= origin <> "url" .= url <> "links" .= links

data FetchResult
    = Failure
    | Success !Record

data WriteCommand
    = StopWriting
    | Write !Record

data LogCommand
    = StopLogging
    | Log !T.Text
