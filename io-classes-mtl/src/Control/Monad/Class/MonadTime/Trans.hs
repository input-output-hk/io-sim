{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadTime.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict

import           Control.Monad.Class.MonadTime

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ExceptT e m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (Lazy.StateT s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (Strict.StateT s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (Lazy.WriterT w m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (Strict.WriterT w m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (Lazy.RWST r w s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (Strict.RWST r w s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ContT r m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadTime m => MonadTime (ExceptT e m) where
  getCurrentTime   = lift getCurrentTime

instance MonadTime m => MonadTime (Lazy.StateT s m) where
  getCurrentTime = lift getCurrentTime

instance MonadTime m => MonadTime (Strict.StateT s m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (Lazy.WriterT w m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (Strict.WriterT w m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (Lazy.RWST r w s m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (Strict.RWST r w s m) where
  getCurrentTime = lift getCurrentTime

instance MonadTime m => MonadTime (ContT r m) where
  getCurrentTime   = lift getCurrentTime
