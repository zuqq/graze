module Graze.Robots.Types
    ( Robots
    , Rule (..)
    , RuleType (..)
    )
    where

import Data.Text (Text)

data RuleType = Disallow | Allow
    deriving Eq

instance Semigroup RuleType where
    Allow    <> _ = Allow
    Disallow <> x = x

data Rule = Rule !RuleType !Text
    deriving Eq

-- | If we fix our user agent, then a robots.txt file amounts to a predicate
-- that is @True@ for paths that we are allowed to crawl and @False@ for the
-- others.
type Robots = String -> Bool
