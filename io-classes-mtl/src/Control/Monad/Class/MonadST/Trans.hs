{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Class.MonadST.Trans () where

import           Control.Monad.Cont (ContT)
import           Control.Monad.Except (ExceptT)
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict

import           Control.Monad.Class.MonadST


instance MonadST m => MonadST (ContT r m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (ExceptT e m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Lazy.RWST r w s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Strict.RWST r w s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (Lazy.StateT s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (Strict.StateT s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (Lazy.WriterT w m) where
   withLiftST f = withLiftST $ \g -> f (lift . g) 

instance (Monoid w, MonadST m) => MonadST (Strict.WriterT w m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)
