{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadTime.SI.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Class.MonadTime.Trans ()
import           Control.Monad.Class.MonadTime.SI

instance MonadMonotonicTime m => MonadMonotonicTime (ExceptT e m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadMonotonicTime m => MonadMonotonicTime (StateT s m) where
  getMonotonicTime = lift getMonotonicTime

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (WriterT w m) where
  getMonotonicTime = lift getMonotonicTime

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (RWST r w s m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadMonotonicTime m => MonadMonotonicTime (ContT r m) where
  getMonotonicTime = lift getMonotonicTime
