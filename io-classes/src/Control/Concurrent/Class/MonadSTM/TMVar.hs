{-# LANGUAGE ExplicitNamespaces #-}

-- | This module corresponds to `Control.Concurrent.STM.TMVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.TMVar
  ( -- * MonadSTM
    type TMVar
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
  ) where

import Control.Monad.Class.MonadSTM.Internal
