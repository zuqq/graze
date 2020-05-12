{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl (HttpUrl (..), hash, serialize) where

import qualified Data.Text               as T  (Text)
import qualified Data.Text.Lazy          as TL (fromStrict)
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)

import Data.Digest.Pure.SHA (sha1, showDigest)

-- $setup
-- >>> :set -XOverloadedStrings


data HttpUrl = HttpUrl
    { huScheme :: !T.Text
    , huDomain :: !T.Text
    , huPath   :: !T.Text
    }

-- |
-- >>> x = HttpUrl "http:" "//x" "/y"
-- >>> y = HttpUrl "https:" "//x" "/y"
-- >>> x == y
-- True
instance Eq HttpUrl where
    x == y = (huDomain x, huPath x) == (huDomain y, huPath y)

-- |
-- >>> x = HttpUrl "https:" "//x" "/"
-- >>> y = HttpUrl "http:" "//x" "/y"
-- >>> x < y
-- True
instance Ord HttpUrl where
    x <= y = (huDomain x, huPath x) <= (huDomain y, huPath y)

serialize :: HttpUrl -> T.Text
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

hash :: HttpUrl -> String
hash = showDigest . sha1 . TL.encodeUtf8 . TL.fromStrict . serialize
