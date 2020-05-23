{-# LANGUAGE OverloadedStrings #-}

module Graze.SExpr
    ( SExpr (..)
    , toByteString
    ) where

import qualified Data.ByteString as B (ByteString, intercalate)


data SExpr = Leaf B.ByteString | Node [SExpr]

parens :: B.ByteString -> B.ByteString
parens s = "(" <> s <> ")"

doubleQuotes :: B.ByteString -> B.ByteString
doubleQuotes s = "\"" <> s <> "\""

hsep :: [B.ByteString] -> B.ByteString
hsep = B.intercalate " "

toByteString :: SExpr -> B.ByteString
toByteString (Leaf s)  = doubleQuotes s
toByteString (Node xs) = parens . hsep . fmap toByteString $ xs
