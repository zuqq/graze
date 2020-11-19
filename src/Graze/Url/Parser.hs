{-# LANGUAGE OverloadedStrings #-}

module Graze.Url.Parser
    ( parseUrl
    , parseRelUrl
    ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        ((<=<))
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isAlphaNum)
import           Data.Functor         (($>))
import qualified Data.Text            as T

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
split :: T.Text -> (T.Text, T.Text)
split = T.span (\w -> w /= ';' && w /= '?')

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
folder :: T.Text -> T.Text
folder = T.dropWhileEnd (/= '/') . fst . split

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
    go (_ : xs)    [".."] = reverse ("" : xs)
    go xs          ["."]  = reverse ("" : xs)
    go xs          [""]   = reverse ("" : xs)
    go xs          []     = reverse xs
    -- Handle "..".
    go (".." : xs) (".." : ys) = go (".." : ".." : xs) ys
    go (_ : xs)    (".." : ys) = go xs ys
    go []          (".." : ys) = go [".."] ys
    -- Handle ".".
    go [] ("." : ys) = go ["."] ys
    go xs ("." : ys) = go xs ys
    -- Handle "".
    go xs ("" : ys) = go xs ys
    -- Generic case.
    go xs (y : ys) = go (y : xs) ys

isSchar :: Char -> Bool
isSchar w = isAlphaNum w || w == '+' || w == '-' || w == '.'

scheme :: A.Parser T.Text
scheme = T.snoc <$> A.takeWhile1 isSchar <*> A.char ':'

domain :: A.Parser T.Text
domain = T.append <$> A.string "//" <*> A.takeWhile (/= '/')

path :: A.Parser T.Text
path = T.cons <$> A.char '/' <*> A.takeText

absolute :: A.Parser (T.Text, T.Text, T.Text)
absolute = (,,) <$> scheme <*> A.option "" domain <*> A.takeText

relativeTo :: Url -> A.Parser (T.Text, T.Text, T.Text)
relativeTo (Url x y z) = absolute
    <|> A.endOfInput $> (x, y, z)                -- Matches "".
    <|> (,,) x <$> domain <*> A.takeText         -- Matches "//...".
    <|> (,,) x y <$> path                        -- Matches "/...".
    <|> (,,) x y . (folder z <>) <$> A.takeText  -- Matches everything.

stripFragment :: T.Text -> T.Text
stripFragment = T.takeWhile (/= '#')

check :: (T.Text, T.Text, T.Text) -> Either String Url
check = fmap pack . (checkDomain <=< checkScheme)
  where
    checkScheme (x, y, z) = if x == "http:" || x == "https:"
        then Right (x, y, z)
        else Left "Invalid scheme."
    checkDomain (x, y, z) = if "//" `T.isPrefixOf` y
        then Right (x, y, z)
        else Left "Invalid netloc."
    pack (x, y, z) = Url x y (normalize z)

-- | Parse an absolute HTTP(S) URL.
parseUrl :: T.Text -> Either String Url
parseUrl = check <=< A.parseOnly absolute . stripFragment

-- | Parse an absolute or relative HTTP(S) URL, with the first argument as the
-- base URL.
parseRelUrl :: Url -> T.Text -> Either String Url
parseRelUrl x = check <=< A.parseOnly (relativeTo x) . stripFragment
