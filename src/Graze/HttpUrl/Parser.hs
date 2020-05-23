{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl.Parser
    ( parse
    , parseRelTo
    ) where

import           Control.Applicative         ((<|>), liftA2, liftA3)
import qualified Data.Attoparsec.ByteString  as A
import qualified Data.ByteString             as B
import           Data.Char                   (ord)
import           Data.Word                   (Word8)

import Graze.HttpUrl.Internal (HttpUrl (..))

-- $setup
-- >>> :set -XOverloadedStrings


colon, pound, question, slash :: Word8
colon    = fromIntegral (ord ':')
pound    = fromIntegral (ord '#')
question = fromIntegral (ord '?')
slash    = fromIntegral (ord '/')

scheme :: A.Parser B.ByteString
scheme = liftA2 B.snoc (A.takeWhile (/= colon)) (A.word8 colon) >>= \case
    "http:"  -> return "http:"
    "https:" -> return "https:"
    _        -> fail "scheme"

domain :: A.Parser B.ByteString
domain = liftA2 (<>) (A.string "//") (A.takeWhile (/= slash))

-- | Split a path @"a?b#c"@ into @("a", "?b")@.
--
-- ==== __Examples__
-- >>> split "a?b#c"
-- ("a","?b")
-- >>> split "a?b"
-- ("a","?b")
-- >>> split "?b"
-- ("","?b")
-- >>> split "a#c"
-- ("a","")
-- >>> split "#c"
-- ("","")
-- >>> split "a"
-- ("a","")
-- >>> split ""
-- ("","")
split :: B.ByteString -> (B.ByteString, B.ByteString)
split = B.span (/= question) . fst . B.span (/= pound)

-- | Remove the fragment and any relative path elements; add an initial slash
-- if not present.
--
-- ==== __Examples__
-- >>> normalize "../../../g"
-- "/g"
-- >>> normalize "/./g"
-- "/g"
-- >>> normalize "./../g"
-- "/g"
-- >>> normalize "g?y/./x"
-- "/g?y/./x"
normalize :: B.ByteString -> B.ByteString
normalize s =
    let (p, q)  = split s
        chunks  = filter (not . B.null) . B.split slash $ p
        suffix  = if "/" `B.isSuffixOf` p && chunks /= [] then "/" else ""
        chunks' = go [] chunks
    in "/" <> B.intercalate "/" chunks' <> suffix <> q
  where
    go xs (".." : ys) = go (drop 1 xs) ys
    go xs ("." : ys)  = go xs ys
    go xs (y : ys)    = go (y : xs) ys
    go xs []          = reverse xs

path :: A.Parser B.ByteString
path = normalize <$> liftA2 B.cons (A.word8 slash) A.takeByteString

url :: A.Parser HttpUrl
url = liftA3 HttpUrl scheme domain (path <|> pure "/")

relPath :: B.ByteString -> A.Parser B.ByteString
relPath f = normalize <$> liftA2 (<>) (pure f) A.takeByteString

-- Map a path @"/a/b/c"@ to @"/a/b/"@.
--
-- ==== __Examples__
--
-- >>> folder "/a/b/c"
-- "/a/b/"
-- >>> folder "/a/x"
-- "/a/"
-- >>> folder "/a/"
-- "/a/"
-- >>> folder "/"
-- "/"
-- >>> folder ""
-- ""
folder :: B.ByteString -> B.ByteString
folder = fst . B.spanEnd (/= slash) . fst . split

relTo :: HttpUrl -> A.Parser HttpUrl
relTo HttpUrl {..} = liftA3 HttpUrl
    (scheme <|> pure huScheme)
    (domain <|> pure huDomain)
    (path   <|> relPath (folder huPath))

-- | Parse an absolute HTTP(S) URL.
parse :: B.ByteString -> Either String HttpUrl
parse = A.parseOnly url

-- | Parse an HTTP(S) URL relative to the first argument.
parseRelTo :: HttpUrl -> B.ByteString -> Either String HttpUrl
parseRelTo x = A.parseOnly (relTo x)
