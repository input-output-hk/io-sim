-- undecidable instances needed for 'ContTSTM' instances of
-- 'MonadThrow' and 'MonadCatch' type classes.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Control.Monad.Class.MonadTimer.Trans () where

import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (ExceptT (..))
import Control.Monad.RWS.Lazy qualified as Lazy
import Control.Monad.RWS.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict

import Control.Monad.Class.MonadTimer

import Control.Monad.Class.MonadSTM.Trans ()

instance MonadDelay m => MonadDelay (ContT r m) where
  threadDelay = lift . threadDelay

instance (Monoid w, MonadDelay m) => MonadDelay (Lazy.WriterT w m) where
  threadDelay = lift . threadDelay

instance (Monoid w, MonadDelay m) => MonadDelay (Strict.WriterT w m) where
  threadDelay = lift . threadDelay

instance MonadDelay m => MonadDelay (Lazy.StateT s m) where
  threadDelay = lift . threadDelay

instance MonadDelay m => MonadDelay (Strict.StateT s m) where
  threadDelay = lift . threadDelay

instance MonadDelay m => MonadDelay (ExceptT e m) where
  threadDelay = lift . threadDelay

instance (Monoid w, MonadDelay m) => MonadDelay (Lazy.RWST r w s m) where
  threadDelay = lift . threadDelay

instance (Monoid w, MonadDelay m) => MonadDelay (Strict.RWST r w s m) where
  threadDelay = lift . threadDelay

instance (Monoid w, MonadTimer m) => MonadTimer (Lazy.WriterT w m) where
  registerDelay = lift . registerDelay
  timeout d f   = Lazy.WriterT $ do
    r <- timeout d (Lazy.runWriterT f)
    return $ case r of
      Nothing     -> (Nothing, mempty)
      Just (a, w) -> (Just a, w)

instance (Monoid w, MonadTimer m) => MonadTimer (Strict.WriterT w m) where
  registerDelay = lift . registerDelay
  timeout d f   = Strict.WriterT $ do
    r <- timeout d (Strict.runWriterT f)
    return $ case r of
      Nothing     -> (Nothing, mempty)
      Just (a, w) -> (Just a, w)

instance MonadTimer m => MonadTimer (Lazy.StateT s m) where
  registerDelay = lift . registerDelay
  timeout d f = Lazy.StateT $ \s -> do
    r <- timeout d (Lazy.runStateT f s)
    return $ case r of
      Nothing      -> (Nothing, s)
      Just (a, s') -> (Just a, s')

instance MonadTimer m => MonadTimer (Strict.StateT s m) where
  registerDelay = lift . registerDelay
  timeout d f = Strict.StateT $ \s -> do
    r <- timeout d (Strict.runStateT f s)
    return $ case r of
      Nothing      -> (Nothing, s)
      Just (a, s') -> (Just a, s')

instance (Monoid w, MonadTimer m) => MonadTimer (Lazy.RWST r w s m) where
  registerDelay = lift . registerDelay
  timeout d (Lazy.RWST f) = Lazy.RWST $ \r s -> do
    res <- timeout d (f r s)
    return $ case res of
      Nothing         -> (Nothing, s, mempty)
      Just (a, s', w) -> (Just a, s', w)

instance (Monoid w, MonadTimer m) => MonadTimer (Strict.RWST r w s m) where
  registerDelay = lift . registerDelay
  timeout d (Strict.RWST f) = Strict.RWST $ \r s -> do
    res <- timeout d (f r s)
    return $ case res of
      Nothing         -> (Nothing, s, mempty)
      Just (a, s', w) -> (Just a, s', w)
