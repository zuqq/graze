module Graze.Robots.Trie
    (
    -- * Type
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

-- | A generic map.
data Trie a b = Trie !(Map a (Trie a b)) !(Maybe b)

-- | The empty trie.
empty :: (Ord a, Semigroup b) => Trie a b
empty = Trie mempty mempty

-- | Insert a key-value pair into the trie.
--
-- Replaces the value @u@ previously stored at the key by @u <> Just value@.
insert :: (Ord a, Semigroup b) => [a] -> b -> Trie a b -> Trie a b
insert key value = go key
  where
    go [] (Trie ts u)       = Trie ts (u <> Just value)
    go (x : xs) (Trie ts u) =
        Trie
            (Map.insert
                x
                (go xs (Map.findWithDefault empty x ts))
                ts)
            u

-- | Find the value associated with the maximal prefix of the given key that is
-- stored in @t@.
--
-- ==== __Examples__
--
-- >>> let t = insert "ab" "y" (insert "a" "x" empty)
-- >>> findMostSpecific "abc" t
-- Just "y"
-- >>> let t' = insert "ab" "0" t
-- >>> findMostSpecific "abc" t'
-- Just "y0"
findMostSpecific :: Ord a  => [a] -> Trie a b -> Maybe b
findMostSpecific key = getLast . go (Last Nothing) key
  where
    go result (x : xs) (Trie ts u)
        | Just t <- Map.lookup x ts = go (result <> Last u) xs t
    go result _ (Trie _ u)          = result <> Last u
