{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl (HttpUrl (..)) where

import qualified Data.Text as T (Text, unpack)

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

-- |
-- >>> HttpUrl "http:" "//x" "/y"
-- http://x/y
instance Show HttpUrl where
    show HttpUrl {..} = T.unpack $ huScheme <> huDomain <> huPath
