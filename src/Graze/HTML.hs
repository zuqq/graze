{-# LANGUAGE OverloadedStrings #-}

module Graze.HTML (parseLinks) where

import Data.Foldable (find, foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Text.HTML.Parser

import qualified Data.Set as Set
import qualified Data.Text as Text

import Graze.URI

parseLinks :: URI -> Text -> Set URI
parseLinks base = foldl' step mempty . parseTokens
  where
    step uris (TagOpen "a" attrs) =
        fromMaybe uris (do
            Attr _ value <- find (\(Attr name _) -> name == "href") attrs
            uri <- parseURIRelativeTo base (Text.unpack value)
            pure (Set.insert uri uris))
    step uris _                   = uris
