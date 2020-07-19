{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl.Parser
    ( parse
    , parseRel
    ) where

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
isAlpha w = 65 <= w && w <= 90  -- 'A'..'Z'
    || 97 <= w && w <= 122      -- 'a'..'z'

isNum :: Word8 -> Bool
isNum w = 48 <= w && w <= 57  -- '0'..'9'

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

-- | Remove the query and the last @'/'@-separated part of that path.
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
-- If the given string doesn't start with a slash, one is added.
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
normalize :: B.ByteString -> B.ByteString
normalize s =
    let (p, q) = split s
        chunks = case B.split slash p of
            "" : ys -> ys  -- Remove leading slash.
            ys      -> ys
    in "/" <> B.intercalate "/" (go [] chunks) <> q
  where
    -- Base cases.
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

-- Parsers ---------------------------------------------------------------------

-- The definitions are roughly those of RFC 1808.

scheme :: A.Parser B.ByteString
scheme = liftA2 (<>) (A.takeWhile1 isScheme) (A.string ":")

netLoc :: A.Parser B.ByteString
netLoc = liftA2 (<>) (A.string "//") (A.takeWhile (/= slash))

relPath :: A.Parser B.ByteString
relPath = A.takeWhile (/= pound)

nothing :: A.Parser B.ByteString
nothing = pure ""

absPath :: A.Parser B.ByteString
absPath = liftA2 (<>) (A.string "/") relPath
    <|> (A.endOfInput *> nothing)

type Url = (B.ByteString, B.ByteString, B.ByteString)

absUrl :: A.Parser Url
absUrl = liftA3 (,,) scheme netLoc absPath
    <|> liftA3 (,,) scheme nothing absPath
    <|> liftA3 (,,) scheme nothing relPath

relUrl :: Url -> A.Parser Url
relUrl (x, y, z) = absUrl
    -- Special case: the relative link @""@ should resolve to the base URL.
    <|> const (x, y, z) <$> (A.endOfInput *> nothing)
    -- Other relative links.
    <|> liftA2 ((,,) x) netLoc absPath        -- Matches "//...".
    <|> (,,) x y <$> absPath                  -- Matches "/...".
    <|> (,,) x y . (folder z <>) <$> relPath  -- Matches everything.

-- Interface -------------------------------------------------------------------

fromUrl :: Url -> Either String HttpUrl
fromUrl (x, y, z) =
    if (x == "http:" || x == "https:") && "//" `B.isPrefixOf` y
        then Right (HttpUrl x y (normalize z))
        else Left "Not a valid HTTP(S) URL."

-- | Parse an absolute HTTP(S) URL.
parse :: B.ByteString -> Either String HttpUrl
parse = A.parseOnly absUrl >=> fromUrl

-- | Parse an HTTP(S) URL, with the first argument as the base URL.
parseRel :: HttpUrl -> B.ByteString -> Either String HttpUrl
parseRel HttpUrl {..} = A.parseOnly (relUrl base) >=> fromUrl
  where
    base = (huScheme, huDomain, huPath)
