-- | This module corresponds to "Control.Monad.STM" in "stm" package
--
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- undecidable instances needed for 'WrappedSTM' instances of 'MonadThrow' and
-- 'MonadCatch' type classes.
{-# LANGUAGE UndecidableInstances  #-}
module Control.Monad.Class.MonadSTM
  ( MonadSTM (STM, atomically, retry, orElse, check)
  , throwSTM
    -- * non standard extensions
    --
    -- $non-standard-extensions
  , MonadLabelledSTM
  , MonadTraceSTM (..)
  , TraceValue (..)
  , MonadInspectSTM (..)
  ) where

import           Control.Monad.Class.MonadSTM.Internal

-- $non-standard-extensions
--
-- The non standard extensions include `MonadLabelledSTM` and `MonadTraceSTM` /
-- `MonadInspectSTM`.  For `IO` these are all no-op, however they greatly
-- enhance [`IOSim`](https://hackage.haskell.org/package/io-sim) capabilities.
-- They are not only useful for debugging concurrency issues, but also to write
-- testable properties.
