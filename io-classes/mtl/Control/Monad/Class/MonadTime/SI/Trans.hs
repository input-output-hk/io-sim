{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Class.MonadTime.SI.Trans () where

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS.Lazy qualified as Lazy
import Control.Monad.RWS.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict

import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTime.Trans ()

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
