{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Class.MonadST.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Class.MonadST


instance MonadST m => MonadST (ContT r m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (ExceptT e m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (RWST r w s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (StateT s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (WriterT w m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)
