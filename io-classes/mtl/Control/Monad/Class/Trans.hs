-- | Export all orphaned instances.
--
module Control.Monad.Class.Trans (module X) where

import Control.Monad.Class.MonadEventlog.Trans as X ()
import Control.Monad.Class.MonadSay.Trans as X ()
import Control.Monad.Class.MonadST.Trans as X ()
import Control.Monad.Class.MonadSTM.Trans as X
import Control.Monad.Class.MonadThrow.Trans as X ()
import Control.Monad.Class.MonadTime.SI.Trans as X ()
import Control.Monad.Class.MonadTime.Trans as X ()
import Control.Monad.Class.MonadTimer.SI.Trans as X ()
import Control.Monad.Class.MonadTimer.Trans as X ()
