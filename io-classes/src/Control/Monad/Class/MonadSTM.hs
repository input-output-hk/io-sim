-- | This module corresponds to `Control.Monad.STM` in "stm" package
--
module Control.Monad.Class.MonadSTM
  ( MonadSTM (STM, atomically, retry, orElse, check)
  , throwSTM
    -- * non standard extensions
  , MonadLabelledSTM
  , MonadTraceSTM
  , MonadInspectSTM (..)
  , TraceValue (..)
    -- * monad transformer 'STM' wrapper
  , WrappedSTM (..)
  ) where

import           Control.Monad.Class.MonadSTM.Internal
