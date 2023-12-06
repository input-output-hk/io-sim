{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE BangPatterns #-}

module Data.TrieMap.Class where

import           Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as Set

-- This module defined the TrieMap class as it is defined in ghc package
-- GHC.Data.TrieMap and adds more functions to the original type class.
--
-- Look at https://simon.peytonjones.org/assets/pdfs/triemaps-that-match.pdf
-- for an up to date introduction to implementing Trie Maps out of algebraic
-- data types.

class Functor m => TrieMap m where
   type Key m :: Type
   emptyTM        :: m a
   lookupTM       :: forall b. Key m -> m b -> Maybe b
   alterTM        :: forall b. Key m -> (Maybe b -> Maybe b) -> m b -> m b
   filterTM       :: (a -> Bool) -> m a -> m a
   foldTM         :: (a -> b -> b) -> m a -> b -> b
   unionWithTM    :: (a -> a -> a) -> m a -> m a -> m a
   assocsTM       :: m a -> [(Key m, a)]

insertTM :: TrieMap m => Key m -> a -> m a -> m a
insertTM k v = alterTM k (\_ -> Just v)

deleteTM :: TrieMap m => Key m -> m a -> m a
deleteTM k = alterTM k (const Nothing)

adjustTM :: TrieMap m => (a -> a) -> Key m -> m a -> m a
adjustTM f k = alterTM k (fmap f)

foldMapTM :: (TrieMap m, Monoid r) => (a -> r) -> m a -> r
foldMapTM f m = foldTM (\ !x !r -> f x <> r) m mempty

isEmptyTM :: TrieMap m => m a -> Bool
isEmptyTM m = foldTM (\ _ _ -> False) m True

elemsTM :: TrieMap m => m a -> [a]
elemsTM m = foldTM (:) m [ ]

memberTM :: TrieMap m => Key m -> m a -> Bool
memberTM k m = isJust $ lookupTM k m

notMemberTM :: TrieMap m => Key m -> m a -> Bool
notMemberTM k m = not $ memberTM k m

keysSetTM :: (TrieMap m, Ord (Key m)) => m a -> Set (Key m)
keysSetTM = Set.fromList . map fst . assocsTM

fromListWithTM :: TrieMap m => (a -> a -> a) -> [(Key m, a)] -> m a
fromListWithTM f =
  foldl' (\acc (k, a) ->
            alterTM k (\case
                          Nothing -> Just a
                          Just x  -> Just (f x a)
                      )
                      acc
         )
         emptyTM

fromListTM :: TrieMap m => [(Key m, a)] -> m a
fromListTM = foldl' (\acc (k, a) -> insertTM k a acc) emptyTM

-- TrieMap instances for containers Map types
--

instance TrieMap IntMap.IntMap where
  type Key IntMap.IntMap = Int
  emptyTM        = IntMap.empty
  alterTM        = flip IntMap.alter
  foldTM k m z   = IntMap.foldr k z m
  filterTM       = IntMap.filter
  lookupTM       = IntMap.lookup
  unionWithTM    = IntMap.unionWith
  assocsTM       = IntMap.assocs

instance Ord k => TrieMap (Map.Map k) where
  type Key (Map.Map k) = k
  emptyTM        = Map.empty
  lookupTM       = Map.lookup
  foldTM k m z   = Map.foldr k z m
  alterTM        = flip Map.alter
  filterTM       = Map.filter
  unionWithTM    = Map.unionWith
  assocsTM       = Map.assocs
