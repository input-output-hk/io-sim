{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Class.MonadTimer
  ( MonadDelay (..)
  , MonadTimer (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Concurrent.Class.MonadSTM
import qualified Control.Concurrent.STM.TVar as STM

import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans (lift)

import qualified System.Timeout as IO

class Monad m => MonadDelay m where
  threadDelay :: Int -> m ()

class (MonadDelay m, MonadSTM m) => MonadTimer m where

  registerDelay :: Int -> m (TVar m Bool)

  timeout :: Int -> m a -> m (Maybe a)

--
-- Instances for IO
--

instance MonadDelay IO where
  threadDelay = IO.threadDelay


instance MonadTimer IO where

  registerDelay = STM.registerDelay
  timeout = IO.timeout

--
-- Transformer's instances
--

instance MonadDelay m => MonadDelay (ReaderT r m) where
  threadDelay = lift . threadDelay

instance MonadTimer m => MonadTimer (ReaderT r m) where
  registerDelay = lift . registerDelay
  timeout d f   = ReaderT $ \r -> timeout d (runReaderT f r)
