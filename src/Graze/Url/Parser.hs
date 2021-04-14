{-# LANGUAGE OverloadedStrings #-}

module Graze.Url.Parser
    ( parseUrl
    , parseRelUrl
    )
    where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text

import Graze.Url.Types (Url (Url))

-- $setup
-- >>> :set -XOverloadedStrings

-- | Split on the first occurrence of @\';\'@ or @\'?\'@.
--
-- ==== __Examples__
--
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
split :: Text -> (Text, Text)
split = Text.span (\w -> w /= ';' && w /= '?')

-- | Remove parameters, query, and the last @\'/\'@-separated part of the path.
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
folder :: Text -> Text
folder = Text.dropWhileEnd (/= '/') . fst . split

-- | Remove relative path elements.
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
normalize :: Text -> Text
normalize s =
    let (p, q) = split s
        chunks = case Text.split (== '/') p of
            "" : ys -> ys  -- Remove leading slash.
            ys      -> ys
    in "/" <> Text.intercalate "/" (go [] chunks) <> q
  where
    -- Base cases.
    -- Trailing ".." needs to be handled separately because it adds '/'.
    go (".." : xs) [".."] = reverse ("" : ".." : ".." : xs)
    go (_ : xs)    [".."] = reverse ("" : xs)
    go xs          ["."]  = reverse ("" : xs)
    go xs          [""]   = reverse ("" : xs)
    go xs          []     = reverse xs
    -- Handle "..".
    go (".." : xs) (".." : ys) = go (".." : ".." : xs) ys
    go (_ : xs)    (".." : ys) = go xs                 ys
    go []          (".." : ys) = go [".."]             ys
    -- Handle ".".
    go [] ("." : ys) = go ["."] ys
    go xs ("." : ys) = go xs    ys
    -- Handle "".
    go xs ("" : ys) = go xs ys
    -- Generic case.
    go xs (y : ys) = go (y : xs) ys

isSchar :: Char -> Bool
isSchar w = isAlphaNum w || w == '+' || w == '-' || w == '.'

scheme :: Attoparsec.Parser Text
scheme = Text.snoc <$> Attoparsec.takeWhile1 isSchar <*> Attoparsec.char ':'

domain :: Attoparsec.Parser Text
domain = Text.append <$> "//" <*> Attoparsec.takeWhile (/= '/')

path :: Attoparsec.Parser Text
path = Text.cons <$> Attoparsec.char '/' <*> Attoparsec.takeText

absolute :: Attoparsec.Parser (Text, Text, Text)
absolute =
        (,,)
    <$> scheme
    <*> Attoparsec.option "" domain  -- Make sure to match "mailto:...".
    <*> Attoparsec.takeText

relativeTo :: Url -> Attoparsec.Parser (Text, Text, Text)
relativeTo (Url x y z) =
        absolute
    <|> Attoparsec.endOfInput $> (x, y, z)                -- Matches "".
    <|> (,,) x <$> domain <*> Attoparsec.takeText         -- Matches "//...".
    <|> (,,) x y <$> path                                 -- Matches "/...".
    <|> (,,) x y . (folder z <>) <$> Attoparsec.takeText  -- Matches everything.

stripFragment :: Text -> Text
stripFragment = Text.takeWhile (/= '#')

check :: (Text, Text, Text) -> Either String Url
check = fmap pack . (checkDomain <=< checkScheme)
  where
    checkScheme (x, y, z) = if x == "http:" || x == "https:"
        then Right (x, y, z)
        else Left "Invalid scheme."
    checkDomain (x, y, z) = if "//" `Text.isPrefixOf` y
        then Right (x, y, z)
        else Left "Invalid netloc."
    pack (x, y, z) = Url x y (normalize z)

-- | Parse an absolute HTTP(S) URL.
parseUrl :: Text -> Either String Url
parseUrl = check <=< Attoparsec.parseOnly absolute . stripFragment

-- | Parse an absolute or relative HTTP(S) URL, with the first argument as the
-- base URL.
parseRelUrl :: Url -> Text -> Either String Url
parseRelUrl x = check <=< Attoparsec.parseOnly (relativeTo x) . stripFragment
