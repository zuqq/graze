{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- module Graze.HttpUrl.Parser
--     ( parse
--     , parseRel
--     ) where

module Graze.HttpUrl.Parser where

import           Control.Applicative         ((<|>), liftA2, liftA3)
import           Control.Monad               ((>=>))
import qualified Data.Attoparsec.ByteString  as A
import qualified Data.ByteString             as B
import           Data.Word                   (Word8)

import Graze.HttpUrl.Internal (HttpUrl (..))

-- $setup
-- >>> :set -XOverloadedStrings


-- Characters ------------------------------------------------------------------

pound, question, slash :: Word8
pound    = 35  -- '#'
question = 63  -- '?'
slash    = 47  -- '/'

isAlpha :: Word8 -> Bool
isAlpha w = 65 <= w && w <= 90 || 97 <= w && w <= 122

isNum :: Word8 -> Bool
isNum w = 48 <= w && w <= 57

isScheme :: Word8 -> Bool
isScheme w = isAlpha w || isNum w
    || w == 43  -- '+'
    || w == 45  -- '-'
    || w == 46  -- '.'

-- Helpers ---------------------------------------------------------------------

-- | Split on the first occurrence of @'?'@.
--
-- ==== __Examples__
-- >>> split "a?b"
-- ("a","?b")
-- >>> split "?b"
-- ("","?b")
-- >>> split "a"
-- ("a","")
-- >>> split ""
-- ("","")
split :: B.ByteString -> (B.ByteString, B.ByteString)
split = B.span (/= question)

-- | Remove the last @'/'@-separated part.
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

-- | Remove relative path elements.
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

-- Parsers ---------------------------------------------------------------------

-- The definitions are roughly those of RFC 1808.

scheme :: A.Parser B.ByteString
scheme = liftA2 (<>) (A.takeWhile1 isScheme) (A.string ":")

netLoc :: A.Parser B.ByteString
netLoc = liftA2 (<>) (A.string "//") (A.takeWhile (/= slash))

relPath :: A.Parser B.ByteString
relPath = A.takeWhile (/= pound)

absPath :: A.Parser B.ByteString
absPath = liftA2 (<>) (A.string "/") relPath
    <|> (A.endOfInput *> pure "/")

type Url = (B.ByteString, B.ByteString, B.ByteString)

absUrl :: A.Parser Url
absUrl = liftA3 (,,) scheme netLoc absPath
    -- Hack for filtering out schemes other than HTTP(S) later.
    <|> liftA3 (,,) scheme nothing nothing
  where
    nothing = pure ""

relUrl :: Url -> A.Parser Url
relUrl (x, y, z) = absUrl
    <|> liftA2 ((,,) x) netLoc absPath
    <|> (,,) x y <$> absPath
    <|> (,,) x y . (folder z <>) <$> relPath

-- Interface -------------------------------------------------------------------

fromUrl :: Url -> Either String HttpUrl
fromUrl (x, y, z) =
    if (x == "http:" || x == "https:") && "//" `B.isPrefixOf` y
        then Right (HttpUrl x y z)
        else Left "Not a valid HTTP(S) URL."

-- | Parse an absolute HTTP(S) URL.
parse :: B.ByteString -> Either String HttpUrl
parse = A.parseOnly absUrl >=> fromUrl

-- | Parse an HTTP(S) URL, with the first argument as the base URL.
parseRel :: HttpUrl -> B.ByteString -> Either String HttpUrl
parseRel HttpUrl {..} = A.parseOnly (relUrl base) >=> fromUrl
  where
    base = (huScheme, huDomain, huPath)