module Graze.Robots.Trie (empty, fromList, member, Trie) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM


data Trie a = Trie
    { end      :: !Bool
    , children :: !(HM.HashMap a (Trie a))
    }

empty :: Trie a
empty = Trie False HM.empty

-- | Check whether the 'Trie' contains a prefix of the given list.
member :: (Eq a, Hashable a) => [a] -> Trie a -> Bool
member _ t
    | end t = True
member [] _ = False
member (x : xs) t = case HM.lookup x (children t) of
    Nothing -> False
    Just t' -> xs `member` t'

insert :: (Eq a, Hashable a) => [a] -> Trie a -> Trie a
insert [] t       = t { end = True }
insert (x : xs) t = t { children = c' }
  where
    c  = children t
    t' = HM.lookupDefault empty x c
    c' = HM.insert x (insert xs t') c

fromList :: (Eq a, Hashable a) => [[a]] -> Trie a
fromList = foldr insert empty
