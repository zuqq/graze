module Graze.Robots.Types
    ( Robots
    , Rule (..)
    , RuleType (..)
    , UserAgent
    )
    where

import Data.CaseInsensitive (CI)
import Data.Text (Text)

type UserAgent = CI Text

data RuleType = Disallow | Allow

instance Semigroup RuleType where
    Allow    <> _ = Allow
    Disallow <> x = x

data Rule = Rule !RuleType !Text

-- | If we fix our user agent, then a robots.txt file amounts to a predicate
-- that is @True@ for paths that we are allowed to crawl and @False@ for the
-- others.
type Robots = String -> Bool
