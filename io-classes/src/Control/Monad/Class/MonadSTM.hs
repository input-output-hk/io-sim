-- | This module corresponds to `Control.Monad.STM` in "stm" package
--
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- undecidable instances needed for 'WrappedSTM' instances of 'MonadThrow' and
-- 'MonadCatch' type classes.
{-# LANGUAGE UndecidableInstances       #-}
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
