{-# LANGUAGE TypeFamilies #-}

module Control.Concurrent.Class.MonadQSem
  ( MonadQSem (..)
  ) where

import Control.Concurrent.QSem qualified as IO

import Data.Kind (Type)

class Monad m => MonadQSem m where
  {-# MINIMAL newQSem, waitQSem, signalQSem #-}

  type QSem m :: Type

  -- | See 'IO.newQSem.
  newQSem    :: Int -> m (QSem m)
  -- | See 'IO.waitQSem'.
  waitQSem   :: QSem m -> m ()
  -- | See 'IO.signalQSem'.
  signalQSem :: QSem m -> m ()

--
-- IO instance
--

instance MonadQSem IO where
  type QSem IO = IO.QSem

  newQSem      = IO.newQSem
  waitQSem     = IO.waitQSem
  signalQSem   = IO.signalQSem
