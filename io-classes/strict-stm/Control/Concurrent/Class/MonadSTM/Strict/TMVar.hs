{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}

-- | This module corresponds to `Control.Concurrent.STM.TMVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TMVar
  ( -- * StrictTMVar
    StrictTMVar
  , LazyTMVar
  , toLazyTMVar
  , fromLazyTMVar
  , castStrictTMVar
  , newTMVar
  , newEmptyTMVar
  , newTMVarIO
  , newEmptyTMVarIO
  , takeTMVar
  , tryTakeTMVar
  , putTMVar
  , tryPutTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , writeTMVar
  , isEmptyTMVar
    -- * MonadLabelledSTM
  , labelTMVar
  , labelTMVarIO
    -- * MonadTraceSTM
  , traceTMVar
  , traceTMVarIO
  , debugTraceTMVar
  , debugTraceTMVarIO
  ) where


import Control.Concurrent.Class.MonadSTM.TMVar qualified as Lazy
import Control.Monad.Class.MonadSTM hiding (traceTMVar, traceTMVarIO)


type LazyTMVar   m = Lazy.TMVar m

-- | 'TMVar' that keeps its value in WHNF at all times
newtype StrictTMVar m a = StrictTMVar { toLazyTMVar :: LazyTMVar m a }

fromLazyTMVar :: LazyTMVar m a -> StrictTMVar m a
fromLazyTMVar = StrictTMVar

labelTMVar :: MonadLabelledSTM m => StrictTMVar m a -> String -> STM m ()
labelTMVar (StrictTMVar tvar) = Lazy.labelTMVar tvar

labelTMVarIO :: MonadLabelledSTM m => StrictTMVar m a -> String -> m ()
labelTMVarIO v = atomically . labelTMVar v

traceTMVar :: MonadTraceSTM m
           => proxy m
           -> StrictTMVar m a
           -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
           -> STM m ()
traceTMVar p (StrictTMVar var) = Lazy.traceTMVar p var

debugTraceTMVar :: (MonadTraceSTM m, Show a)
               => proxy m
               -> StrictTMVar m a
               -> STM m ()
debugTraceTMVar p (StrictTMVar var) = Lazy.debugTraceTMVar p var

traceTMVarIO :: MonadTraceSTM m
             => StrictTMVar m a
             -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
             -> m ()
traceTMVarIO (StrictTMVar var) = Lazy.traceTMVarIO var

debugTraceTMVarIO :: (MonadTraceSTM m, Show a)
                 => StrictTMVar m a
                 -> m ()
debugTraceTMVarIO (StrictTMVar var) = Lazy.debugTraceTMVarIO var

castStrictTMVar :: LazyTMVar m ~ LazyTMVar n
                => StrictTMVar m a -> StrictTMVar n a
castStrictTMVar (StrictTMVar var) = StrictTMVar var

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = StrictTMVar <$> Lazy.newTMVar a

newTMVarIO :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarIO !a = StrictTMVar <$> Lazy.newTMVarIO a

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = StrictTMVar <$> Lazy.newEmptyTMVar

newEmptyTMVarIO :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarIO = StrictTMVar <$> Lazy.newEmptyTMVarIO

takeTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
takeTMVar (StrictTMVar tmvar) = Lazy.takeTMVar tmvar

tryTakeTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryTakeTMVar (StrictTMVar tmvar) = Lazy.tryTakeTMVar tmvar

putTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m ()
putTMVar (StrictTMVar tmvar) !a = Lazy.putTMVar tmvar a

tryPutTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m Bool
tryPutTMVar (StrictTMVar tmvar) !a = Lazy.tryPutTMVar tmvar a

readTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
readTMVar (StrictTMVar tmvar) = Lazy.readTMVar tmvar

tryReadTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryReadTMVar (StrictTMVar tmvar) = Lazy.tryReadTMVar tmvar

swapTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m a
swapTMVar (StrictTMVar tmvar) !a = Lazy.swapTMVar tmvar a

writeTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m ()
writeTMVar (StrictTMVar tmvar) !a = Lazy.writeTMVar tmvar a

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar (StrictTMVar tmvar) = Lazy.isEmptyTMVar tmvar
