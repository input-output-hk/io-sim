-- undecidable instances needed for 'ContTSTM' instances of
-- 'MonadThrow' and 'MonadCatch' type classes.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Control.Monad.Class.MonadTimer.SI.Trans () where

import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (ExceptT (..))
import Control.Monad.RWS (RWST (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..))

import Control.Monad.Class.MonadTimer.SI

import Control.Monad.Class.MonadTime.SI.Trans ()
import Control.Monad.Class.MonadTimer.Trans ()

import Data.Bifunctor (bimap)


instance MonadDelay m => MonadDelay (ContT r m) where
  threadDelay = lift . threadDelay
instance (Monoid w, MonadDelay m) => MonadDelay (WriterT w m) where
  threadDelay = lift . threadDelay
instance MonadDelay m => MonadDelay (StateT s m) where
  threadDelay = lift . threadDelay
instance MonadDelay m => MonadDelay (ExceptT e m) where
  threadDelay = lift . threadDelay
instance (Monoid w, MonadDelay m) => MonadDelay (RWST r w s m) where
  threadDelay = lift . threadDelay

instance (Monoid w, MonadTimer m) => MonadTimer (WriterT w m) where
  registerDelay            = lift . registerDelay
  registerDelayCancellable = fmap (bimap lift lift)
                           . lift
                           . registerDelayCancellable
  timeout d f   = WriterT $ do
    r <- timeout d (runWriterT f)
    return $ case r of
      Nothing     -> (Nothing, mempty)
      Just (a, w) -> (Just a, w)

instance MonadTimer m => MonadTimer (StateT s m) where
  registerDelay            = lift . registerDelay
  registerDelayCancellable = fmap (bimap lift lift)
                           . lift
                           . registerDelayCancellable
  timeout d f = StateT $ \s -> do
    r <- timeout d (runStateT f s)
    return $ case r of
      Nothing      -> (Nothing, s)
      Just (a, s') -> (Just a, s')

instance (Monoid w, MonadTimer m) => MonadTimer (RWST r w s m) where
  registerDelay            = lift . registerDelay
  registerDelayCancellable = fmap (bimap lift lift)
                           . lift
                           . registerDelayCancellable
  timeout d (RWST f) = RWST $ \r s -> do
    res <- timeout d (f r s)
    return $ case res of
      Nothing         -> (Nothing, s, mempty)
      Just (a, s', w) -> (Just a, s', w)

