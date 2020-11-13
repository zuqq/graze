-- | A generic trie implementation for lists of 'Hashable' values.

module Graze.Trie
    ( Trie
    , completes
    , empty
    , fromList
    , insert
    ) where

import           Data.Foldable       (foldl')
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM


-- | A trie @t :: Trie a@ stores lists @xs :: [a]@ as paths in a tree, where
-- the elements of @xs@ label the edges on the path. Every node carries a
-- boolean flag that indicates whether it marks the end of a list.
--
-- A trie lets us efficiently determine whether one of the lists it stores is a
-- prefix of a given list @ys :: [a]@.
data Trie a = Trie !Bool !(HM.HashMap a (Trie a))

-- | The empty trie.
--
-- ==== __Properties__
--
-- prop> fmap (`completes` empty) (xs :: [String]) == fmap (const False) xs
empty :: Trie a
empty = Trie False HM.empty

-- | Insert an item into the trie.
insert :: (Eq a, Hashable a) => [a] -> Trie a -> Trie a
insert [] (Trie _ ts)          = Trie True ts
insert (x : xs) (Trie flag ts) = Trie flag ts'
  where
    t   = HM.lookupDefault empty x ts
    ts' = HM.insert x (insert xs t) ts

-- | Build a trie from a list of items.
fromList :: (Eq a, Hashable a) => [[a]] -> Trie a
fromList = foldl' (flip insert) empty

-- | @xs \`completes\` t@ is @True@ if and only if @t@ contains a prefix of @xs@.
--
-- ==== __Examples__
--
-- >>> t = fromList ["to", "tea", "ted", "ten", "inn"]
-- >>> "too" `completes` t
-- True
-- >>> "teapot" `completes` t
-- True
-- >>> "tan" `completes` t
-- False
-- >>> "xavier" `completes` t
-- False
-- >>> "nanoparticle" `completes` t
-- False
-- >>> t' = insert "nano" t
-- >>> "nanoparticle" `completes` t'
-- True
--
-- ==== __Properties__
--
-- prop> fmap (`completes` fromList xs) (xs :: [String]) == fmap (const True) xs
completes :: (Eq a, Hashable a) => [a] -> Trie a -> Bool
completes _ (Trie True _)      = True
completes [] _                 = False
completes (x : xs) (Trie _ ts) = case HM.lookup x ts of
    Nothing -> False
    Just t  -> xs `completes` t
