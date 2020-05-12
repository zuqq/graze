module Graze.Trie (completes, fromList, Trie) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM


data Trie a = Trie !Bool !(HM.HashMap a (Trie a))

empty :: Trie a
empty = Trie False HM.empty

insert :: (Eq a, Hashable a) => [a] -> Trie a -> Trie a
insert [] (Trie _ ts)          = Trie True ts
insert (x : xs) (Trie flag ts) = Trie flag ts'
  where
    t'  = HM.lookupDefault empty x ts
    ts' = HM.insert x (insert xs t') ts

fromList :: (Eq a, Hashable a) => [[a]] -> Trie a
fromList = foldr insert empty

-- | @xs `completes` t@ is @True@ if and only if @t@ contains a prefix of @xs@.
completes :: (Eq a, Hashable a) => [a] -> Trie a -> Bool
completes _ (Trie True _)      = True
completes [] _                 = False
completes (x : xs) (Trie _ ts) = case HM.lookup x ts of
    Nothing -> False
    Just t' -> xs `completes` t'
