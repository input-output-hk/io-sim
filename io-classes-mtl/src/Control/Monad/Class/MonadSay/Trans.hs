{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Class.MonadSay.Trans () where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.Class.MonadSay

-- | @since 0.1.0.0
instance MonadSay m => MonadSay (ContT r m) where
  say  = lift . say

-- | @since 0.1.0.0
instance MonadSay m => MonadSay (ExceptT e m) where
  say  = lift . say

-- | @since 0.1.0.0
instance (Monoid w, MonadSay m) => MonadSay (RWST r w s m) where
  say  = lift . say

-- | @since 0.1.0.0
instance MonadSay m => MonadSay (StateT s m) where
  say  = lift . say

-- | @since 0.1.0.0
instance (Monoid w, MonadSay m) => MonadSay (WriterT w m) where
  say  = lift . say
