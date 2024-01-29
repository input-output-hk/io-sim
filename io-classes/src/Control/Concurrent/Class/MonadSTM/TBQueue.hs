{-# LANGUAGE ExplicitNamespaces #-}

-- | This module corresponds to `Control.Concurrent.STM.TVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.TBQueue
  ( -- * MonadSTM
    type TBQueue
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

import Control.Monad.Class.MonadSTM.Internal
