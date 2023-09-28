{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}

-- | This module corresponds to `Control.Concurrent.STM.TBQueue` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TBQueue
  ( -- * MonadSTM
    StrictTBQueue
  , LazyTBQueue
  , toLazyTBQueue
  , fromLazyTBQueue
  , castStrictTBQueue
  , newTBQueue
  , newTBQueueIO
  , readTBQueue
  , tryReadTBQueue
  , peekTBQueue
  , tryPeekTBQueue
  , flushTBQueue
  , writeTBQueue
  , lengthTBQueue
  , isEmptyTBQueue
  , isFullTBQueue
  , unGetTBQueue
    -- * MonadLabelledSTM
  , labelTBQueue
  , labelTBQueueIO
    -- * MonadTraceSTM
  , traceTBQueue
  , traceTBQueueIO
  ) where


import Control.Concurrent.Class.MonadSTM.TBQueue qualified as Lazy
import Control.Monad.Class.MonadSTM hiding (traceTBQueue, traceTBQueueIO)

import Numeric.Natural (Natural)


type LazyTBQueue m = Lazy.TBQueue m

newtype StrictTBQueue m a = StrictTBQueue { toLazyTBQueue :: LazyTBQueue m a }

fromLazyTBQueue :: LazyTBQueue m a -> StrictTBQueue m a
fromLazyTBQueue = StrictTBQueue

castStrictTBQueue :: LazyTBQueue m ~ LazyTBQueue n
                  => StrictTBQueue m a -> StrictTBQueue n a
castStrictTBQueue (StrictTBQueue var) = StrictTBQueue var

labelTBQueue :: MonadLabelledSTM m => StrictTBQueue m a -> String -> STM m ()
labelTBQueue (StrictTBQueue queue) = Lazy.labelTBQueue queue

labelTBQueueIO :: MonadLabelledSTM m => StrictTBQueue m a -> String -> m ()
labelTBQueueIO (StrictTBQueue queue) = Lazy.labelTBQueueIO queue

traceTBQueue :: MonadTraceSTM m
             => proxy m
             -> StrictTBQueue m a
             -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
             -> STM m ()
traceTBQueue p (StrictTBQueue queue) = Lazy.traceTBQueue p queue

traceTBQueueIO :: MonadTraceSTM m
               => StrictTBQueue m a
               -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
               -> m ()
traceTBQueueIO (StrictTBQueue queue) = Lazy.traceTBQueueIO queue

newTBQueue :: MonadSTM m => Natural -> STM m (StrictTBQueue m a)
newTBQueue n = StrictTBQueue <$> Lazy.newTBQueue n

newTBQueueIO :: MonadSTM m => Natural -> m (StrictTBQueue m a)
newTBQueueIO = atomically . newTBQueue

readTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m a
readTBQueue = Lazy.readTBQueue . toLazyTBQueue

tryReadTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m (Maybe a)
tryReadTBQueue = Lazy.tryReadTBQueue . toLazyTBQueue

peekTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m a
peekTBQueue = Lazy.peekTBQueue . toLazyTBQueue

tryPeekTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m (Maybe a)
tryPeekTBQueue = Lazy.tryPeekTBQueue . toLazyTBQueue

flushTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m [a]
flushTBQueue = Lazy.flushTBQueue . toLazyTBQueue

writeTBQueue :: MonadSTM m => StrictTBQueue m a -> a -> STM m ()
writeTBQueue (StrictTBQueue tqueue) !a = Lazy.writeTBQueue tqueue a

lengthTBQueue  :: MonadSTM m => StrictTBQueue m a -> STM m Natural
lengthTBQueue = Lazy.lengthTBQueue . toLazyTBQueue

isEmptyTBQueue  :: MonadSTM m => StrictTBQueue m a -> STM m Bool
isEmptyTBQueue = Lazy.isEmptyTBQueue . toLazyTBQueue

isFullTBQueue  :: MonadSTM m => StrictTBQueue m a -> STM m Bool
isFullTBQueue = Lazy.isFullTBQueue . toLazyTBQueue

unGetTBQueue :: MonadSTM m => StrictTBQueue m a -> a -> STM m ()
unGetTBQueue (StrictTBQueue queue) !a = Lazy.unGetTBQueue queue a
