module Graze.Trie
    ( Trie
    , completes
    , empty
    , fromList
    , insert
    , toList
    ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import qualified Data.HashSet as HS (fromList)


-- | A trie @t :: Trie a@ stores items @xs :: [a]@ as paths in a tree emenating
-- from the root, with elements of @xs@ becoming labels of the edges on the
-- path. Every node carries a boolean flag that indicates whether it marks the
-- end of an item.
--
-- A trie lets us efficiently determine whether one of the items it stores is a
-- prefix of a given value @ys :: [a]@.
data Trie a = Trie !Bool !(H.HashMap a (Trie a))

-- | The empty trie.
--
-- ==== __Properties__
--
-- prop> \(xs :: [String]) -> map (`completes` empty) xs == map (const False) xs
empty :: Trie a
empty = Trie False H.empty

-- | Insert an item into the trie.
insert :: (Eq a, Hashable a) => [a] -> Trie a -> Trie a
insert [] (Trie _ ts)          = Trie True ts
insert (x : xs) (Trie flag ts) = Trie flag ts'
  where
    t   = H.lookupDefault empty x ts
    ts' = H.insert x (insert xs t) ts

-- | Build a trie from a list of items.
fromList :: (Eq a, Hashable a) => [[a]] -> Trie a
fromList = foldr insert empty

-- | Build a list of items from a trie.
--
-- ==== __Properties__
--
-- prop> \(xs :: [String]) -> (HS.fromList . toList . fromList $ xs) == HS.fromList xs
toList :: Trie a -> [[a]]
toList = go [] []
  where
    go acc path (Trie flag ts) = H.foldrWithKey
        (\k v xs -> go xs (k : path) v)
        (if flag then reverse path : acc else acc)
        ts

-- | The expression @xs `completes` t@ is @True@ if and only if @t@ contains a
-- prefix of @xs@.
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
-- prop> \(xs :: [String]) -> map (`completes` fromList xs) xs == map (const True) xs
completes :: (Eq a, Hashable a) => [a] -> Trie a -> Bool
completes _ (Trie True _)      = True
completes [] _                 = False
completes (x : xs) (Trie _ ts) = case H.lookup x ts of
    Nothing -> False
    Just t  -> xs `completes` t
