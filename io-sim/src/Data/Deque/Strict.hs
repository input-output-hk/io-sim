{-# LANGUAGE LambdaCase #-}

-- | A minimal implementation of a strict deque.
--
module Data.Deque.Strict where

import           Prelude hiding (head, init, tail)
import           Data.Foldable (foldl', foldr')
import qualified Data.List as List

data Deque a = Deque ![a] ![a]

instance Semigroup (Deque a) where
    Deque as bs <> Deque as' bs' =
      Deque as (bs' ++ reverse as' ++ bs)

instance Monoid (Deque a) where
    mempty = Deque [] []

instance Foldable Deque where
    foldr step init (Deque head tail) =
      foldr step (foldl' (flip step) init tail) head
    foldl' step init (Deque head tail) =
      foldr' (flip step) (foldl' step init head) tail

fromList :: [a] -> Deque a
fromList as = Deque as []

snoc :: a -> Deque a -> Deque a
snoc a (Deque as bs) = Deque as (a : bs)

uncons :: Deque a -> Maybe (a, Deque a)
uncons = \case
    Deque (a : head') tail -> Just (a, Deque head' tail)
    Deque [] tail ->
      case reverse tail of
        (a : head') -> Just (a, Deque head' [])
        []          -> Nothing

filter :: (a -> Bool) -> Deque a -> Deque a
filter f (Deque head tail) = Deque (List.filter f head) (List.filter f tail)
