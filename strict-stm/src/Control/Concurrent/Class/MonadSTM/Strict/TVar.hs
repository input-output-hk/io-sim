{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | This module corresponds to `Control.Concurrent.STM.TVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TVar
  ( -- * StrictTVar
    StrictTVar
  , LazyTVar
  , toLazyTVar
  , fromLazyTVar
  , castStrictTVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , stateTVar
  , swapTVar
  , check
    -- * MonadLabelSTM
  , labelTVar
  , labelTVarIO
    -- * MonadTraceSTM
  , traceTVar
  , traceTVarIO
  ) where

import Control.Concurrent.Class.MonadSTM.TVar qualified as Lazy
import Control.Monad.Class.MonadSTM hiding (traceTVar, traceTVarIO)

type LazyTVar m = Lazy.TVar m

newtype StrictTVar m a = StrictTVar {
    tvar :: LazyTVar m a
  }

labelTVar :: MonadLabelledSTM m => StrictTVar m a -> String -> STM m ()
labelTVar StrictTVar { tvar } = Lazy.labelTVar tvar

labelTVarIO :: MonadLabelledSTM m => StrictTVar m a -> String -> m ()
labelTVarIO v = atomically . labelTVar v

traceTVar :: MonadTraceSTM m
          => proxy m
          -> StrictTVar m a
          -> (Maybe a -> a -> InspectMonad m TraceValue)
          -> STM m ()
traceTVar p StrictTVar {tvar} = Lazy.traceTVar p tvar

traceTVarIO :: MonadTraceSTM m
            => StrictTVar m a
            -> (Maybe a -> a -> InspectMonad m TraceValue)
            -> m ()
traceTVarIO StrictTVar {tvar} = Lazy.traceTVarIO tvar

-- | Cast the monad if both use the same representation of `TVar`s.
--
-- This function is useful for monad transformers stacks if the `TVar` is used
-- in different monad stacks.
--
castStrictTVar :: LazyTVar m ~ LazyTVar n
               => StrictTVar m a -> StrictTVar n a
castStrictTVar StrictTVar {tvar} = StrictTVar {tvar}

-- | Get the underlying @TVar@
--
-- Since we obviously cannot guarantee that updates to this 'LazyTVar' will be
-- strict, this should be used with caution.
toLazyTVar :: StrictTVar m a -> LazyTVar m a
toLazyTVar StrictTVar { tvar } = tvar

fromLazyTVar :: LazyTVar m a -> StrictTVar m a
fromLazyTVar = StrictTVar

newTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
newTVar !a = StrictTVar <$> Lazy.newTVar a

newTVarIO :: MonadSTM m => a -> m (StrictTVar m a)
newTVarIO !a = StrictTVar <$> Lazy.newTVarIO a

readTVar :: MonadSTM m => StrictTVar m a -> STM m a
readTVar StrictTVar { tvar } = Lazy.readTVar tvar

readTVarIO :: MonadSTM m => StrictTVar m a -> m a
readTVarIO StrictTVar { tvar } = Lazy.readTVarIO tvar

writeTVar :: MonadSTM m => StrictTVar m a -> a -> STM m ()
writeTVar v !a = Lazy.writeTVar (tvar v) a

modifyTVar :: MonadSTM m => StrictTVar m a -> (a -> a) -> STM m ()
modifyTVar v f = readTVar v >>= writeTVar v . f

stateTVar :: MonadSTM m => StrictTVar m s -> (s -> (a, s)) -> STM m a
stateTVar v f = do
    a <- readTVar v
    let (b, a') = f a
    writeTVar v a'
    return b

swapTVar :: MonadSTM m => StrictTVar m a -> a -> STM m a
swapTVar v a' = do
    a <- readTVar v
    writeTVar v a'
    return a
