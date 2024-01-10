module Control.Monad.Class.MonadTest (MonadTest (..)) where

import           Control.Monad.Reader

-- | A helper monad for /IOSimPOR/.
class Monad m => MonadTest m where
  -- | Mark a thread for schedule exploration.  All threads that are forked by
  -- it are also included in the exploration.
  --
  exploreRaces :: m ()
  exploreRaces = return ()

instance MonadTest IO

instance MonadTest m => MonadTest (ReaderT e m) where
  exploreRaces = lift exploreRaces

