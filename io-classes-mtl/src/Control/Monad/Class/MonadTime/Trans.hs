{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadTime.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Class.MonadTime

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ExceptT e m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (StateT s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (WriterT w m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (RWST r w s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ContT r m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadTime m => MonadTime (ExceptT e m) where
  getCurrentTime   = lift getCurrentTime

instance MonadTime m => MonadTime (StateT s m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (WriterT w m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (RWST r w s m) where
  getCurrentTime = lift getCurrentTime

instance MonadTime m => MonadTime (ContT r m) where
  getCurrentTime   = lift getCurrentTime
