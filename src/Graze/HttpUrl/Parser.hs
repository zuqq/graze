{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graze.HttpUrl.Parser (parse, parseRel) where

import           Control.Applicative  ((<|>), liftA2, liftA3)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

import Graze.HttpUrl (HttpUrl(..))


scheme :: A.Parser T.Text
scheme = A.string "https:" <|> A.string "http:"

domain :: A.Parser T.Text
domain = liftA2 (<>) (A.string "//") (A.takeWhile (/= '/'))

path :: A.Parser T.Text
path = liftA2 T.cons (A.char '/') A.takeText

url :: A.Parser HttpUrl
url = liftA3 HttpUrl scheme domain (path <|> pure "/")

relUrl :: HttpUrl -> A.Parser HttpUrl
relUrl HttpUrl {..} = liftA3 HttpUrl
    (scheme <|> pure huScheme)
    (domain <|> pure huDomain)
    (path   <|> relPath)
  where
    huFolder = T.dropWhileEnd (/= '/') huPath
    relPath  = liftA2 (<>) (pure huFolder) A.takeText

parse :: T.Text -> Either String HttpUrl
parse = A.parseOnly url

parseRel :: HttpUrl -> T.Text -> Either String HttpUrl
parseRel base = A.parseOnly (relUrl base)
