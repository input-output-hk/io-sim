{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadTime.SI.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict

import           Control.Monad.Class.MonadTime.Trans ()
import           Control.Monad.Class.MonadTime.SI

instance MonadMonotonicTime m => MonadMonotonicTime (ExceptT e m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadMonotonicTime m => MonadMonotonicTime (Lazy.StateT s m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadMonotonicTime m => MonadMonotonicTime (Strict.StateT s m) where
  getMonotonicTime = lift getMonotonicTime

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (Lazy.WriterT w m) where
  getMonotonicTime = lift getMonotonicTime

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (Strict.WriterT w m) where
  getMonotonicTime = lift getMonotonicTime

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (Lazy.RWST r w s m) where
  getMonotonicTime = lift getMonotonicTime

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (Strict.RWST r w s m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadMonotonicTime m => MonadMonotonicTime (ContT r m) where
  getMonotonicTime = lift getMonotonicTime
