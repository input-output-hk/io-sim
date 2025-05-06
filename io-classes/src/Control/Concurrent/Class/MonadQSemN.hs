{-# LANGUAGE TypeFamilies #-}

module Control.Concurrent.Class.MonadQSemN
  ( MonadQSemN (..)
  ) where

import Control.Concurrent.QSemN qualified as IO

import Data.Kind (Type)

class Monad m => MonadQSemN m where
  {-# MINIMAL newQSemN, waitQSemN, signalQSemN #-}

  type QSemN m :: Type

  -- | See 'IO.newQSemN.
  newQSemN    :: Int -> m (QSemN m)
  -- | See 'IO.waitQSemN'.
  waitQSemN   :: QSemN m -> Int -> m ()
  -- | See 'IO.signalQSemN'.
  signalQSemN :: QSemN m -> Int -> m ()

--
-- IO instance
--

instance MonadQSemN IO where
  type QSemN IO = IO.QSemN

  newQSemN      = IO.newQSemN
  waitQSemN     = IO.waitQSemN
  signalQSemN   = IO.signalQSemN

