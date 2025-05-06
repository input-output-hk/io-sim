{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Concurrent.Class.MonadChan
  ( MonadChan (..)
  ) where

import Control.Concurrent.Chan qualified as IO

import Data.Kind (Type)

class Monad m => MonadChan m where
  {-# MINIMAL newChan,
              writeChan, readChan,
              dupChan, getChanContents #-}

  type Chan m :: Type -> Type

  -- | See 'IO.newChan.
  newChan      :: m (Chan m a)
  -- | See 'IO.writeChan'.
  writeChan    :: Chan m a -> a -> m ()
  -- | See 'IO.readChan'.
  readChan     :: Chan m a -> m a
  -- | See 'IO.dupChan'.
  dupChan      :: Chan m a -> m (Chan m a)
  -- | See 'IO.getChanContents'.
  getChanContents :: Chan m a -> m [a]
  -- | See 'IO.writeList2Chan'
  writeList2Chan :: Chan m a -> [a] -> m ()

  default writeList2Chan :: Chan m a -> [a] -> m ()
  writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)
  {-# INLINE writeList2Chan #-}

--
-- IO instance
--

instance MonadChan IO where
  type Chan IO    = IO.Chan

  newChan         = IO.newChan
  writeChan       = IO.writeChan
  readChan        = IO.readChan
  dupChan         = IO.dupChan
  getChanContents = IO.getChanContents
