-- | This module corresponds to "Control.Concurrent.STM" in "stm" package
--
module Control.Concurrent.Class.MonadSTM (module STM) where

import Control.Concurrent.Class.MonadSTM.TArray as STM
import Control.Concurrent.Class.MonadSTM.TBQueue as STM
import Control.Concurrent.Class.MonadSTM.TChan as STM
import Control.Concurrent.Class.MonadSTM.TMVar as STM
import Control.Concurrent.Class.MonadSTM.TQueue as STM
import Control.Concurrent.Class.MonadSTM.TVar as STM
import Control.Monad.Class.MonadSTM as STM

