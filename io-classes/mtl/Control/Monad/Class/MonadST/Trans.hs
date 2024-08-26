{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Control.Monad.Class.MonadST.Trans () where

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS.Lazy qualified as Lazy
import Control.Monad.RWS.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict

import Control.Monad.Class.MonadST


instance MonadST m => MonadST (ContT r m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (ExceptT e m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Lazy.RWST r w s m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Strict.RWST r w s m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (Lazy.StateT s m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (Strict.StateT s m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Lazy.WriterT w m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Strict.WriterT w m) where
  stToIO = lift . stToIO
  withLiftST f = withLiftST $ \g -> f (lift . g)
