{-# LANGUAGE OverloadedStrings #-}

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

import Data.Aeson ((.=), ToJSON (..), object, pairs)

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
    { rOrigin :: !HttpUrl
    , rUrl    :: !HttpUrl
    , rLinks  :: ![HttpUrl]
    }

instance ToJSON Record where
    toJSON (Record origin url links)     =
        object ["origin" .= origin, "url" .= url, "links" .= links]
    toEncoding (Record origin url links) =
        pairs $ "origin" .= origin <> "url" .= url <> "links" .= links

data FetchResult
    = Failure
    | Success !Job !Record !BL.ByteString

data WriteCommand
    = StopWriting
    | Write !Record !BL.ByteString

data LogCommand
    = StopLogging
    | Log !T.Text
