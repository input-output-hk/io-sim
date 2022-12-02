{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Class.MonadTimer
  ( MonadDelay (..)
  , MonadTimer (..)
  , registerDelayCancellable
  , TimeoutState (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Concurrent.Class.MonadSTM
import qualified Control.Concurrent.STM.TVar as STM

import           Control.Monad.Cont (ContT (..))
import           Control.Monad.Except (ExceptT (..))
import           Control.Monad.RWS (RWST (..))
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.State (StateT (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer (WriterT (..))

import           Data.Functor (void)

import           Control.Monad.Class.MonadTimer.NonStandard

import qualified System.Timeout as IO

class Monad m => MonadDelay m where
  threadDelay :: Int -> m ()

  default threadDelay :: MonadTimer m => Int -> m ()
  threadDelay d = void . atomically . awaitTimeout =<< newTimeout d

class (MonadDelay m, MonadTimeout m) => MonadTimer m where

  registerDelay :: Int -> m (TVar m Bool)

  timeout :: Int -> m a -> m (Maybe a)

--
-- Cancellable Timers
--

registerDelayCancellable :: forall m.  MonadTimer m
                         => Int
                         -> m (STM m TimeoutState, m ())

registerDelayCancellable d = do
    t <- newTimeout d
    return (readTimeout t, cancelTimeout t)

--
-- Instances for IO
--

-- | With 'threadDelay' one can use arbitrary large 'DiffTime's, which is an
-- advantage over 'IO.threadDelay'.
--
instance MonadDelay IO where
  threadDelay = IO.threadDelay


instance MonadTimer IO where

  registerDelay = STM.registerDelay
  timeout = IO.timeout

--
-- Transformer's instances
--

instance MonadDelay m => MonadDelay (ContT r m) where
  threadDelay = lift . threadDelay
instance MonadDelay m => MonadDelay (ReaderT r m) where
  threadDelay = lift . threadDelay
instance (Monoid w, MonadDelay m) => MonadDelay (WriterT w m) where
  threadDelay = lift . threadDelay
instance MonadDelay m => MonadDelay (StateT s m) where
  threadDelay = lift . threadDelay
instance MonadDelay m => MonadDelay (ExceptT e m) where
  threadDelay = lift . threadDelay
instance (Monoid w, MonadDelay m) => MonadDelay (RWST r w s m) where
  threadDelay = lift . threadDelay

instance MonadTimer m => MonadTimer (ReaderT r m) where
  registerDelay = lift . registerDelay
  timeout d f   = ReaderT $ \r -> timeout d (runReaderT f r)

instance (Monoid w, MonadTimer m) => MonadTimer (WriterT w m) where
  registerDelay = lift . registerDelay
  timeout d f   = WriterT $ do
    r <- timeout d (runWriterT f)
    return $ case r of
      Nothing     -> (Nothing, mempty)
      Just (a, w) -> (Just a, w)

instance MonadTimer m => MonadTimer (StateT s m) where
  registerDelay = lift . registerDelay
  timeout d f = StateT $ \s -> do
    r <- timeout d (runStateT f s)
    return $ case r of
      Nothing      -> (Nothing, s)
      Just (a, s') -> (Just a, s')

instance (Monoid w, MonadTimer m) => MonadTimer (RWST r w s m) where
  registerDelay = lift . registerDelay
  timeout d (RWST f) = RWST $ \r s -> do
    res <- timeout d (f r s)
    return $ case res of
      Nothing         -> (Nothing, s, mempty)
      Just (a, s', w) -> (Just a, s', w)
