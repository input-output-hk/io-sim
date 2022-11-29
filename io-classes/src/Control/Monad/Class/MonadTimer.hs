{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Control.Monad.Class.MonadTimer
  ( MonadDelay (..)
  , MonadTimer (..)
  , TimeoutState (..)
  , DiffTime
  , diffTimeToMicrosecondsAsInt
  , microsecondsAsIntToDiffTime
  ) where

import qualified Control.Concurrent as IO
import           Control.Concurrent.Class.MonadSTM.TVar
import qualified Control.Concurrent.STM.TVar as STM

import           Control.Monad.Cont (ContT (..))
import           Control.Monad.Except (ExceptT (..))
import           Control.Monad.RWS (RWST (..))
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.State (StateT (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer (WriterT (..))

import           Data.Functor (void)
import           Data.Time.Clock (DiffTime)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer.NonStandard

import qualified System.Timeout as IO

class Monad m => MonadDelay m where
  threadDelay :: DiffTime -> m ()

  default threadDelay :: MonadTimer m => DiffTime -> m ()
  threadDelay d   = void . atomically . awaitTimeout =<< newTimeout d

class (MonadDelay m, MonadTimeout m) => MonadTimer m where

  registerDelay :: DiffTime -> m (TVar m Bool)

  default registerDelay :: MonadFork m => DiffTime -> m (TVar m Bool)
  registerDelay = defaultRegisterDelay

  timeout :: DiffTime -> m a -> m (Maybe a)


defaultRegisterDelay :: ( MonadTimer m
                        , MonadFork  m
                        )
                     => DiffTime
                     -> m (TVar m Bool)
defaultRegisterDelay d = do
    v <- atomically $ newTVar False
    t <- newTimeout d
    _ <- forkIO $ atomically (awaitTimeout t >>= writeTVar v)
    return v

--
-- Instances for IO
--

-- | With 'threadDelay' one can use arbitrary large 'DiffTime's, which is an
-- advantage over 'IO.threadDelay'.
--
instance MonadDelay IO where
  threadDelay = go
    where
      go :: DiffTime -> IO ()
      go d | d > maxDelay = do
        IO.threadDelay maxBound
        go (d - maxDelay)
      go d = do
        IO.threadDelay (diffTimeToMicrosecondsAsInt d)

      maxDelay :: DiffTime
      maxDelay = microsecondsAsIntToDiffTime maxBound

instance MonadTimer IO where

  -- | For delays less (or equal) than @maxBound :: Int@ this is exactly the same as
  -- 'STM.registerDaley'; for larger delays it will start a monitoring thread
  -- which will update the 'TVar'.
  --
  -- TODO: issue #2184 'registerDelay' relies on 'newTimeout', through
  -- 'defaultRegisterDelay'.  'newTimeout' can overflow an 'Int' (this is
  -- especially easy on 32-bit architectures).
  registerDelay d
      | d <= maxDelay =
        STM.registerDelay (diffTimeToMicrosecondsAsInt d)
      | otherwise =
        defaultRegisterDelay d
    where
      maxDelay :: DiffTime
      maxDelay = microsecondsAsIntToDiffTime maxBound

  timeout = IO.timeout . diffTimeToMicrosecondsAsInt

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
