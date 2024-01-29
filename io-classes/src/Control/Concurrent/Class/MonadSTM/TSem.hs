{-# LANGUAGE ExplicitNamespaces #-}

-- | This module corresponds to `Control.Concurrent.STM.TSem` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.TSem
  ( -- * MonadSTM
    type TSem
  , newTSem
  , waitTSem
  , signalTSem
  , signalTSemN
    -- * MonadLabelledSTM
  , labelTSem
  , labelTSemIO
    -- * MonadTraceSTM
  , traceTSem
  , traceTSemIO
  ) where

import Control.Monad.Class.MonadSTM.Internal
