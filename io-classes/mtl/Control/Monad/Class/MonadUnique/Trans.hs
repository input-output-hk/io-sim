{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Class.MonadUnique.Trans () where

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS.Lazy qualified as Lazy
import Control.Monad.RWS.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict

import Control.Monad.Class.MonadUnique


instance MonadUnique m => MonadUnique (ContT r m) where
  type Unique (ContT r m) = UniqueFor (ContT r) m

instance MonadUnique m => MonadUnique (ExceptT e m) where
  type Unique (ExceptT e m) = UniqueFor (ExceptT e) m

instance (MonadUnique m, Monoid w) => MonadUnique (Lazy.RWST r w s m) where
  type Unique (Lazy.RWST r w s m) = UniqueFor (Lazy.RWST r w s) m

instance (MonadUnique m, Monoid w) => MonadUnique (Strict.RWST r w s m) where
  type Unique (Strict.RWST r w s m) = UniqueFor (Strict.RWST r w s) m

instance MonadUnique m => MonadUnique (Lazy.StateT s m) where
  type Unique (Lazy.StateT s m) = UniqueFor (Lazy.StateT s) m

instance MonadUnique m => MonadUnique (Strict.StateT s m) where
  type Unique (Strict.StateT s m) = UniqueFor (Strict.StateT s) m

instance (MonadUnique m, Monoid w) => MonadUnique (Lazy.WriterT w m) where
  type Unique (Lazy.WriterT w m) = UniqueFor (Lazy.WriterT w) m

instance (MonadUnique m, Monoid w) => MonadUnique (Strict.WriterT w m) where
  type Unique (Strict.WriterT w m) = UniqueFor (Strict.WriterT w) m
