module Control.Monad.Class.MonadKeepAlive
  ( MonadKeepAlive (..)
  ) where

import Control.Monad.ST as Strict
import Control.Monad.ST.Lazy as Lazy
import Control.Monad.Primitive qualified as IO


class MonadKeepAlive m where
  keepAlive :: a -> m r -> m r

instance MonadKeepAlive IO where
  keepAlive = IO.keepAlive

instance MonadKeepAlive (Strict.ST s) where
  keepAlive = IO.keepAlive

instance MonadKeepAlive (Lazy.ST s) where
  keepAlive = IO.keepAlive
