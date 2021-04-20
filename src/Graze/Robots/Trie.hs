module Graze.Robots.Trie
    (
    -- * Trie type
      Trie
    -- * Construction
    , empty
    -- * Insertion
    , insert
    -- * Query
    , findMostSpecific
    )
    where

import Data.Map.Strict (Map)
import Data.Monoid (Last (..))

import qualified Data.Map.Strict as Map

data Trie a b = Trie !(Map a (Trie a b)) !(Maybe b)

-- | The empty trie.
empty :: (Ord a, Semigroup b) => Trie a b
empty = Trie mempty mempty

-- | Insert a key-value pair into the trie.
--
-- Inserting the value @u@ into the trie replaces the old value stored at the
-- given key by @u <> Just value@.
insert :: (Ord a, Semigroup b) => [a] -> b -> Trie a b -> Trie a b
insert [] value (Trie ts u)       = Trie ts (u <> Just value)
insert (x : xs) value (Trie ts u) = Trie (Map.insert x t ts) u
  where
    t = insert xs value (Map.findWithDefault empty x ts)

-- | @findMostSpecific xs t@ returns the value associated with the maximal
-- prefix of @xs@ that is stored in @t@.
findMostSpecific :: Ord a  => [a] -> Trie a b -> Maybe b
findMostSpecific xs_ = getLast . go (Last Nothing) xs_
  where
    go result (x : xs) (Trie ts u)
        | Just t <- Map.lookup x ts = go (result <> Last u) xs t
    go result _ (Trie _ u)          = result <> Last u
