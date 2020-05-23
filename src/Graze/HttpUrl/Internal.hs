module Graze.HttpUrl.Internal
    ( HttpUrl (..)
    ) where

import qualified Data.ByteString as B (ByteString)
import           Data.Hashable   (Hashable (hashWithSalt))


data HttpUrl = HttpUrl
    { huScheme :: !B.ByteString
    , huDomain :: !B.ByteString
    , huPath   :: !B.ByteString
    }

instance Eq HttpUrl where
    x == y = (huDomain x, huPath x) == (huDomain y, huPath y)

instance Hashable HttpUrl where
    hashWithSalt salt x = hashWithSalt salt (huDomain x <> huPath x)
