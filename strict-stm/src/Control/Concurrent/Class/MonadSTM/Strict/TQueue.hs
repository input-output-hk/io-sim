{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeOperators      #-}

-- | This module corresponds to `Control.Concurrent.STM.TQueue` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TQueue
  ( -- * MonadSTM
    StrictTQueue
  , LazyTQueue
  , toLazyTQueue
  , fromLazyTQueue
  , castStrictTQueue
  , newTQueue
  , newTQueueIO
  , readTQueue
  , tryReadTQueue
  , peekTQueue
  , tryPeekTQueue
  , flushTQueue
  , writeTQueue
  , unGetTQueue
  , isEmptyTQueue
    -- * MonadLabelledSTM
  , labelTQueue
  , labelTQueueIO
    -- * MonadTraceSTM
  , traceTQueue
  , traceTQueueIO
  ) where


import qualified Control.Concurrent.Class.MonadSTM.TQueue as Lazy
import           Control.Monad.Class.MonadSTM hiding (traceTQueue, traceTQueueIO)


type LazyTQueue  m = Lazy.TQueue m

newtype StrictTQueue m a = StrictTQueue { toLazyTQueue :: LazyTQueue m a }

fromLazyTQueue :: LazyTQueue m a -> StrictTQueue m a
fromLazyTQueue = StrictTQueue

castStrictTQueue :: LazyTQueue m ~ LazyTQueue n
                 => StrictTQueue m a -> StrictTQueue n a
castStrictTQueue (StrictTQueue var) = StrictTQueue var

labelTQueue :: MonadLabelledSTM m => StrictTQueue m a -> String -> STM m ()
labelTQueue (StrictTQueue queue) = Lazy.labelTQueue queue

labelTQueueIO :: MonadLabelledSTM m => StrictTQueue m a -> String -> m ()
labelTQueueIO (StrictTQueue queue) = Lazy.labelTQueueIO queue

traceTQueue :: MonadTraceSTM m
            => proxy m
            -> StrictTQueue m a
            -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
            -> STM m ()
traceTQueue p (StrictTQueue queue) = Lazy.traceTQueue p queue

traceTQueueIO :: MonadTraceSTM m
              => StrictTQueue m a
              -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
              -> m ()
traceTQueueIO (StrictTQueue queue) = Lazy.traceTQueueIO queue

newTQueue :: MonadSTM m => STM m (StrictTQueue m a)
newTQueue = StrictTQueue <$> Lazy.newTQueue

newTQueueIO :: MonadSTM m => m (StrictTQueue m a)
newTQueueIO = atomically newTQueue

readTQueue :: MonadSTM m => StrictTQueue m a -> STM m a
readTQueue = Lazy.readTQueue . toLazyTQueue

tryReadTQueue :: MonadSTM m => StrictTQueue m a -> STM m (Maybe a)
tryReadTQueue = Lazy.tryReadTQueue . toLazyTQueue

peekTQueue :: MonadSTM m => StrictTQueue m a -> STM m a
peekTQueue = Lazy.peekTQueue . toLazyTQueue

tryPeekTQueue :: MonadSTM m => StrictTQueue m a -> STM m (Maybe a)
tryPeekTQueue = Lazy.tryPeekTQueue . toLazyTQueue

flushTQueue :: MonadSTM m => StrictTQueue m a -> STM m [a]
flushTQueue = Lazy.flushTQueue . toLazyTQueue

writeTQueue :: MonadSTM m => StrictTQueue m a -> a -> STM m ()
writeTQueue (StrictTQueue tqueue) !a = Lazy.writeTQueue tqueue a

isEmptyTQueue  :: MonadSTM m => StrictTQueue m a -> STM m Bool
isEmptyTQueue = Lazy.isEmptyTQueue . toLazyTQueue

unGetTQueue :: MonadSTM m => StrictTQueue m a -> a -> STM m ()
unGetTQueue (StrictTQueue queue) !a = Lazy.unGetTQueue queue a

