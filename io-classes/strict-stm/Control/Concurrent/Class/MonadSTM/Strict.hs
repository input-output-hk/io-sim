-- | This module corresponds to `Control.Concurrent.STM` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict
  (module STM)
  where

import           Control.Monad.Class.MonadSTM                     as STM hiding ( traceTVar, traceTVarIO,
                                                                                  traceTMVar, traceTMVarIO,
                                                                                  traceTQueue, traceTQueueIO,
                                                                                  traceTBQueue, traceTBQueueIO
                                                                                )
import           Control.Concurrent.Class.MonadSTM.Strict.TVar    as STM
import           Control.Concurrent.Class.MonadSTM.Strict.TMVar   as STM
import           Control.Concurrent.Class.MonadSTM.Strict.TChan   as STM
import           Control.Concurrent.Class.MonadSTM.Strict.TQueue  as STM
import           Control.Concurrent.Class.MonadSTM.Strict.TBQueue as STM
import           Control.Concurrent.Class.MonadSTM.Strict.TArray  as STM


