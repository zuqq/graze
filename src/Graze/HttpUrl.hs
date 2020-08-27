{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Graze.HttpUrl
    ( HttpUrl (..)
    , hashUrl
    , parseUrl
    , parseRelUrl
    , serializeUrl
    ) where

import           Control.Applicative    ((<|>))
import           Control.Monad          ((<=<))
import qualified Data.Attoparsec.Text   as A
import qualified Data.ByteString.Base16 as Base16 (encode)
import qualified Data.ByteString.Char8  as BC (unpack)
import           Data.Char              (isAlphaNum)
import           Data.Functor           (($>))
import           Data.Hashable          (Hashable (hashWithSalt))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T (encodeUtf8)

import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import           Data.Aeson       (ToJSON (..))

-- $setup
-- >>> :set -XOverloadedStrings


-- Type ------------------------------------------------------------------------

data HttpUrl = HttpUrl
    { huScheme :: !T.Text
    , huDomain :: !T.Text
    , huPath   :: !T.Text
    }
    deriving (Read, Show)

instance Eq HttpUrl where
    x == y = (huDomain x, huPath x) == (huDomain y, huPath y)

instance Hashable HttpUrl where
    hashWithSalt salt x = hashWithSalt salt (huDomain x <> huPath x)

-- | Serialize the given 'HttpUrl', by concatenating its constituents.
--
-- ==== __Examples__
--
-- >>> serialize $ HttpUrl "http:" "//www.example.com" "/"
-- "http://www.example.com/"
serializeUrl :: HttpUrl -> T.Text
serializeUrl HttpUrl {..} = huScheme <> huDomain <> huPath

instance ToJSON HttpUrl where
    toJSON     = toJSON . serializeUrl
    toEncoding = toEncoding . serializeUrl

-- | Map an 'HttpUrl' to the base16-encoded SHA-1 digest of its serialization.
--
-- ==== __Examples__
-- >>> hashUrl $ HttpUrl "http:" "//www.example.com" "/"
-- "89e6a0649e06d83370cdf2cbfb05f363934a8d0c"
hashUrl :: HttpUrl -> String
hashUrl = BC.unpack . Base16.encode . SHA1.hash . T.encodeUtf8 . serializeUrl

-- Path ------------------------------------------------------------------------

-- | Split on the first occurrence of @';'@ or @'?'@.
--
-- ==== __Examples__
-- >>> split "a;b?c"
-- ("a",";b?c")
-- >>> split "a;b"
-- ("a",";b")
-- >>> split "a?c"
-- ("a","?c")
-- >>> split "?b"
-- ("","?b")
-- >>> split "a"
-- ("a","")
-- >>> split ""
-- ("","")
split :: T.Text -> (T.Text, T.Text)
split = T.span (\w -> w /= ';' && w /= '?')

-- | Remove parameters, query, and the last @'/'@-separated part of the path.
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
folder :: T.Text -> T.Text
folder = T.dropWhileEnd (/= '/') . fst . split

-- | Remove relative path elements.
--
-- If the input doesn't start with a slash, one is added.
--
-- ==== __Examples__
--
-- >>> normalize "/b/c/../../../g"
-- "/../g"
-- >>> normalize "/b/c/../../../../g"
-- "/../../g"
-- >>> normalize "/./g"
-- "/./g"
-- >>> normalize "/../g"
-- "/../g"
-- >>> normalize "/g."
-- "/g."
-- >>> normalize "/.g"
-- "/.g"
-- >>> normalize "/..g"
-- "/..g"
-- >>> normalize "/g.."
-- "/g.."
-- >>> normalize "/b/c/./../g"
-- "/b/g"
-- >>> normalize "/b/c/./g/."
-- "/b/c/g/"
-- >>> normalize "/b/c/g/./h"
-- "/b/c/g/h"
-- >>> normalize "/b/c/g/../h"
-- "/b/c/h"
normalize :: T.Text -> T.Text
normalize s =
    let (p, q) = split s
        chunks = case T.split (== '/') p of
            "" : ys -> ys  -- Remove leading slash.
            ys      -> ys
    in "/" <> T.intercalate "/" (go [] chunks) <> q
  where
    -- Base cases.
    -- Trailing ".." needs to be handled separately because it adds '/'.
    go (".." : xs) [".."] = reverse ("" : ".." : ".." : xs)
    go (_ : xs) [".."]    = reverse ("" : xs)
    go xs ["."]           = reverse ("" : xs)
    go xs [""]            = reverse ("" : xs)
    go xs []              = reverse xs
    -- Handle "..".
    go (".." : xs) (".." : ys) = go (".." : ".." : xs) ys
    go (_ : xs) (".." : ys)    = go xs ys
    go [] (".." : ys)          = go [".."] ys
    -- Handle ".".
    go [] ("." : ys) = go ["."] ys
    go xs ("." : ys) = go xs ys
    -- Handle "".
    go xs ("" : ys) = go xs ys
    -- Generic case.
    go xs (y : ys) = go (y : xs) ys

-- Url -------------------------------------------------------------------------

type Url = (T.Text, T.Text, T.Text)

toUrl :: HttpUrl -> Url
toUrl (HttpUrl x y z) = (x, y, z)

fromUrl :: Url -> Either String HttpUrl
fromUrl = fmap pack . (checkNetloc <=< checkScheme)
  where
    checkScheme (x, y, z) = if x == "http:" || x == "https:"
        then Right (x, y, z)
        else Left "Invalid scheme."
    checkNetloc (x, y, z) = if "//" `T.isPrefixOf` y
        then Right (x, y, z)
        else Left "Invalid netloc."
    pack (x, y, z) = HttpUrl x y (normalize z)

-- Parsers ---------------------------------------------------------------------

isSchar :: Char -> Bool
isSchar w = isAlphaNum w || w == '+' || w == '-' || w == '.'

-- The definitions are roughly those of RFC 1808.

scheme :: A.Parser T.Text
scheme = T.snoc <$> A.takeWhile1 isSchar <*> A.char ':'

netLoc :: A.Parser T.Text
netLoc = T.append <$> A.string "//" <*> A.takeWhile (/= '/')

absPath :: A.Parser T.Text
absPath = T.cons <$> A.char '/' <*> A.takeText

absUrl :: A.Parser Url
absUrl = (,,) <$> scheme <*> A.option "" netLoc <*> A.takeText

relUrl :: Url -> A.Parser Url
relUrl (x, y, z) = absUrl
    <|> A.endOfInput $> (x, y, z)                -- Matches "".
    <|> (,,) x <$> netLoc <*> A.takeText         -- Matches "//...".
    <|> (,,) x y <$> absPath                     -- Matches "/...".
    <|> (,,) x y . (folder z <>) <$> A.takeText  -- Matches everything.

-- Interface -------------------------------------------------------------------

stripFragment :: T.Text -> T.Text
stripFragment = T.takeWhile (/= '#')

-- | Parse an absolute HTTP(S) URL.
parseUrl :: T.Text -> Either String HttpUrl
parseUrl = fromUrl <=< A.parseOnly absUrl . stripFragment

-- | Parse an absolute or relative HTTP(S) URL, with the first argument as the
-- base URL.
parseRelUrl :: HttpUrl -> T.Text -> Either String HttpUrl
parseRelUrl (toUrl -> x) = fromUrl <=< A.parseOnly (relUrl x) . stripFragment
