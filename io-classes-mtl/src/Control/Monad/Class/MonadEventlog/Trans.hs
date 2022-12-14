{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadEventlog.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Class.MonadEventlog

-- | @since 0.1.0.0
instance MonadEventlog m => MonadEventlog (ContT r m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance MonadEventlog m => MonadEventlog (ExceptT e m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance (Monoid w, MonadEventlog m) => MonadEventlog (RWST r w s m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance MonadEventlog m => MonadEventlog (StateT s m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance (Monoid w, MonadEventlog m) => MonadEventlog (WriterT w m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

