{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadEventlog.Trans () where

import           Control.Monad.Cont (ContT)
import           Control.Monad.Except (ExceptT)
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict

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
instance (Monoid w, MonadEventlog m) => MonadEventlog (Lazy.RWST r w s m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance (Monoid w, MonadEventlog m) => MonadEventlog (Strict.RWST r w s m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance MonadEventlog m => MonadEventlog (Lazy.StateT s m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance MonadEventlog m => MonadEventlog (Strict.StateT s m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance (Monoid w, MonadEventlog m) => MonadEventlog (Lazy.WriterT w m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

-- | @since 0.1.0.0
instance (Monoid w, MonadEventlog m) => MonadEventlog (Strict.WriterT w m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

