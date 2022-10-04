{-# LANGUAGE ExplicitNamespaces #-}

-- | This module corresponds to `Control.Concurrent.STM.TVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.TVar
  ( -- * MonadSTM
    type TVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
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

import           Control.Monad.Class.MonadSTM.Internal
