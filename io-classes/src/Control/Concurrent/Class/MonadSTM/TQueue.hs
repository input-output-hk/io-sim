{-# LANGUAGE ExplicitNamespaces #-}

-- | This module corresponds to `Control.Concurrnet.STM.TVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.TQueue
  ( -- * MonadSTM
    type TQueue
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

import           Control.Monad.Class.MonadSTM.Internal
