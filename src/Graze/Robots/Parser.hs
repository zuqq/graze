{-# LANGUAGE OverloadedStrings #-}

module Graze.Robots.Parser
    ( Line
    , Rule (..)
    , UserAgent
    , parseLine
    ) where

import           Control.Applicative  ((<|>))
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isAscii)
import qualified Data.Text            as T (Text)


type UserAgent = T.Text

data Rule
    = Disallow  !T.Text
    | Allow     !T.Text
    | Extension !T.Text

type Line = Either UserAgent Rule

isSpace :: Char -> Bool
isSpace w = w == ' ' || w == '\t'

isSafe :: Char -> Bool
isSafe w = case w of
    '$' -> True
    '-' -> True
    '_' -> True
    '.' -> True
    '+' -> True
    _   -> False

isExtra :: Char -> Bool
isExtra w = case w of
    '!'  -> True
    '*'  -> True
    '\'' -> True
    '('  -> True
    ')'  -> True
    ','  -> True
    _    -> False

isUchar :: Char -> Bool
isUchar w = w == '%'  -- Treat escaped characters by just allowing '%'.
    || 'a' <= w && w <= 'z'
    || 'A' <= w && w <= 'Z'
    || '0' <= w && w <= '9'
    || isSafe w
    || isExtra w

isPchar :: Char -> Bool
isPchar w = w == '/'  -- Just allow '/' instead of parsing segments.
    || isUchar w
    || w == ':'
    || w == '@'
    || w == '&'
    || w == '='

isCtl :: Char -> Bool
isCtl w = w < '\32' || w == '\127'

isTspecial :: Char -> Bool
isTspecial w = case w of
    '('  -> True
    ')'  -> True
    '<'  -> True
    '>'  -> True
    '@'  -> True
    ','  -> True
    ';'  -> True
    ':'  -> True
    '\\' -> True
    '"'  -> True
    '/'  -> True
    '['  -> True
    ']'  -> True
    '?'  -> True
    '='  -> True
    '{'  -> True
    '}'  -> True
    ' '  -> True
    '\t' -> True
    _    -> False

isTchar :: Char -> Bool
isTchar w = isAscii w && not (isCtl w || isTspecial w)

skipSpace :: A.Parser ()
skipSpace = A.skipWhile isSpace

path :: A.Parser T.Text
path = A.takeWhile isPchar

comment :: A.Parser T.Text
comment = A.char '#' *> A.takeText

userAgent :: A.Parser UserAgent
userAgent = A.string "User-agent:"
    *> skipSpace
    *> A.takeWhile1 isTchar
    <* (A.endOfInput <|> skipSpace <* comment)

disallow :: A.Parser Rule
disallow = fmap Disallow $
    A.string "Disallow:"
    *> skipSpace
    *> path
    <* (A.endOfInput <|> skipSpace <* comment)

allow :: A.Parser Rule
allow = fmap Allow $
    A.string "Allow:"
    *> skipSpace
    *> path
    <* (A.endOfInput <|> skipSpace <* comment)

extension :: A.Parser Rule
extension = fmap Extension $
    A.takeWhile isTchar
    *> A.char ':'
    *> skipSpace
    *> A.takeWhile (/= '#')
    <* (A.endOfInput <|> skipSpace <* comment)

line :: A.Parser Line
line = A.eitherP userAgent (disallow <|> allow <|> extension)

parseLine :: T.Text -> Either String Line
parseLine = A.parseOnly line
