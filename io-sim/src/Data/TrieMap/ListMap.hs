{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module Data.TrieMap.ListMap where

import           Control.Monad ((>=>))
import           Data.TrieMap.Class

-- This module implements ListMaps, as they are defined in ghc package at
-- GHC.Data.TrieMap

data ListMap m a
  = LM { lm_nil  :: !(Maybe a)
       , lm_cons :: !(m (ListMap m a))
       }

instance Functor m => Functor (ListMap m) where
    fmap f LM { lm_nil , lm_cons } =
      LM { lm_nil  = fmap f lm_nil
         , lm_cons = fmap (fmap f) lm_cons
         }

instance TrieMap m => TrieMap (ListMap m) where
   type Key (ListMap m) = [Key m]

   emptyTM :: forall a. ListMap m a
   emptyTM     = LM { lm_nil  = Nothing
                    , lm_cons = emptyTM
                    }

   lookupTM :: forall b. Key (ListMap m) -> ListMap m b -> Maybe b
   lookupTM    = lkList lookupTM

   alterTM :: forall b. Key (ListMap m) -> (Maybe b -> Maybe b) -> ListMap m b -> ListMap m b
   alterTM     = xtList alterTM

   foldTM :: forall a b. (a -> b -> b) -> ListMap m a -> b -> b
   foldTM      = fdList

   filterTM :: forall a. (a -> Bool) -> ListMap m a -> ListMap m a
   filterTM    = ftList

   unionWithTM :: forall a. (a -> a -> a) -> ListMap m a -> ListMap m a -> ListMap m a
   unionWithTM = uWList

   assocsTM :: forall a. ListMap m a -> [(Key (ListMap m), a)]
   assocsTM    = assocsList

assocsList :: TrieMap m => ListMap m a -> [([Key m], a)]
assocsList LM { lm_nil, lm_cons } = nilAssocs ++ consAssocs
  where
    nilAssocs = maybe [] (\ !a -> [([], a)]) lm_nil
    consAssocs = concatMap (\(!k, lm) -> map (prepend k) (assocsList lm))
                           (assocsTM lm_cons)

    prepend !k (!ks, !a) = (k:ks, a)

instance TrieMap m => Foldable (ListMap m) where
  foldMap :: forall m1 a. Monoid m1 => (a -> m1) -> ListMap m a -> m1
  foldMap = foldMapTM

instance (TrieMap m, Show a, Show (Key m)) => Show (ListMap m a) where
  show :: ListMap m a -> String
  show m = "fromListTM " ++ show (assocsList m)

-- Auxiliary functions as defined in ghc package at GHC.Data.TrieMap
--

lkList :: TrieMap m => (forall b. k -> m b -> Maybe b)
       -> [k] -> ListMap m a -> Maybe a
lkList _  []      = lm_nil
lkList lk (!x:xs) = lm_cons >.> lk x >=> lkList lk xs

xtList :: TrieMap m => (forall b. k -> (Maybe b -> Maybe b) -> m b -> m b)
       -> [k] -> (Maybe a -> Maybe a) -> ListMap m a -> ListMap m a
xtList _  []      f m = m { lm_nil  = f (lm_nil m) }
xtList tr (!x:xs) f m = m { lm_cons = lm_cons m |> tr x |>> xtList tr xs f }

fdList :: forall m a b. TrieMap m
       => (a -> b -> b) -> ListMap m a -> b -> b
fdList k m = foldMaybe k          (lm_nil m)
           . foldTM    (fdList k) (lm_cons m)

ftList :: TrieMap m => (a -> Bool) -> ListMap m a -> ListMap m a
ftList f (LM { lm_nil = mnil, lm_cons = mcons })
  = LM { lm_nil = filterMaybe f mnil
       , lm_cons = fmap (\ !x -> filterTM f x) mcons
       }

uWList :: TrieMap m => (a -> a -> a) -> ListMap m a -> ListMap m a -> ListMap m a
uWList f LM { lm_nil = nil1 , lm_cons = cons1 }
         LM { lm_nil = nil2 , lm_cons = cons2 } =
  LM { lm_nil  = case (nil1, nil2) of
        (Just !x, Just !y) -> Just $! f x y
        (_, _)             -> Nothing
     , lm_cons = unionWithTM (\ !x !y -> uWList f x y) cons1 cons2
     }

(>.>) :: (a -> b) -> (b -> c) -> a -> c
-- Reverse function composition (do f first, then g)
infixr 1 >.>
(f >.> g) !x = g (f x)
infixr 1 |>, |>>

(|>) :: a -> (a->b) -> b -- Reverse application
!x |> f = f x

(|>>) :: TrieMap m2
      => ((Maybe (m2 a) -> Maybe (m2 a)) -> m1 (m2 a) -> m1 (m2 a))
      -> (m2 a -> m2 a)
      -> m1 (m2 a) -> m1 (m2 a)
(|>>) f g = f (Just . g . deMaybe)

deMaybe :: TrieMap m => Maybe (m a) -> m a
deMaybe Nothing   = emptyTM
deMaybe (Just !m) = m

foldMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldMaybe _ Nothing  b   = b
foldMaybe k (Just !a) !b = k a b

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f input@(Just x) | f x       = input
                             | otherwise = Nothing
