{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Graze.HttpUrl
    ( HttpUrl (..)
    , hash
    , parse
    , parseRel
    , serialize
    ) where

import           Control.Applicative         ((<|>))
import           Control.Monad               ((<=<))
import qualified Crypto.Hash.SHA1            as SHA1   (hash)
import qualified Data.Attoparsec.ByteString  as A
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base16      as Base16 (encode)
import qualified Data.ByteString.Char8       as C8 (unpack)
import           Data.Functor                (($>))
import           Data.Hashable               (Hashable (hashWithSalt))
import           Data.Word                   (Word8)

import Graze.Word8 (isAlpha, isNum)

-- $setup
-- >>> :set -XOverloadedStrings


-- Type ------------------------------------------------------------------------

data HttpUrl = HttpUrl
    { huScheme :: !B.ByteString
    , huDomain :: !B.ByteString
    , huPath   :: !B.ByteString
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
serialize :: HttpUrl -> B.ByteString
serialize HttpUrl {..} = huScheme <> huDomain <> huPath

-- | Map an 'HttpUrl' to the base16-encoded SHA-1 digest of its serialization.
--
-- ==== __Examples__
-- >>> hash $ HttpUrl "http:" "//www.example.com" "/"
-- "89e6a0649e06d83370cdf2cbfb05f363934a8d0c"
hash :: HttpUrl -> String
hash = C8.unpack . Base16.encode . SHA1.hash . serialize

-- Path ------------------------------------------------------------------------

#define SLASH 47
#define SEMIC 59
#define QUEST 63

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
split :: B.ByteString -> (B.ByteString, B.ByteString)
split = B.span (\w -> w /= SEMIC && w /= QUEST)

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
folder :: B.ByteString -> B.ByteString
folder = fst . B.spanEnd (/= SLASH) . fst . split

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
normalize :: B.ByteString -> B.ByteString
normalize s =
    let (p, q) = split s
        chunks = case B.split SLASH p of
            "" : ys -> ys  -- Remove leading slash.
            ys      -> ys
    in "/" <> B.intercalate "/" (go [] chunks) <> q
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

type Url = (B.ByteString, B.ByteString, B.ByteString)

toUrl :: HttpUrl -> Url
toUrl (HttpUrl x y z) = (x, y, z)

fromUrl :: Url -> Either String HttpUrl
fromUrl = fmap pack . (checkNetloc <=< checkScheme)
  where
    checkScheme (x, y, z) = if x == "http:" || x == "https:"
        then Right (x, y, z)
        else Left "Invalid scheme."
    checkNetloc (x, y, z) = if "//" `B.isPrefixOf` y
        then Right (x, y, z)
        else Left "Invalid netloc."
    pack (x, y, z) = HttpUrl x y (normalize z)

-- Parsers ---------------------------------------------------------------------

#define COLON 58
#define POUND 35

isSchar :: Word8 -> Bool
isSchar w
    | isAlpha w = True
    | isNum w   = True
    | otherwise = case w of
        43 -> True  -- '+'
        45 -> True  -- '-'
        46 -> True  -- '.'
        _  -> False

-- The definitions are roughly those of RFC 1808.

scheme :: A.Parser B.ByteString
scheme = B.snoc <$> A.takeWhile1 isSchar <*> A.word8 COLON

netLoc :: A.Parser B.ByteString
netLoc = B.append <$> A.string "//" <*> A.takeWhile (/= SLASH)

absPath :: A.Parser B.ByteString
absPath = B.cons <$> A.word8 SLASH <*> A.takeByteString

absUrl :: A.Parser Url
absUrl = (,,) <$> scheme <*> A.option "" netLoc <*> A.takeByteString

relUrl :: Url -> A.Parser Url
relUrl (x, y, z) = absUrl
    <|> A.endOfInput $> (x, y, z)                      -- Matches "".
    <|> (,,) x <$> netLoc <*> A.takeByteString         -- Matches "//...".
    <|> (,,) x y <$> absPath                           -- Matches "/...".
    <|> (,,) x y . (folder z <>) <$> A.takeByteString  -- Matches everything.

-- Interface -------------------------------------------------------------------

stripFragment :: B.ByteString -> B.ByteString
stripFragment = B.takeWhile (/= POUND)

-- | Parse an absolute HTTP(S) URL.
parse :: B.ByteString -> Either String HttpUrl
parse = fromUrl <=< A.parseOnly absUrl . stripFragment

-- | Parse an absolute or relative HTTP(S) URL, with the first argument as the
-- base URL.
parseRel :: HttpUrl -> B.ByteString -> Either String HttpUrl
parseRel (toUrl -> x) = fromUrl <=< A.parseOnly (relUrl x) . stripFragment
