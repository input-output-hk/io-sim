{-# LANGUAGE DeriveGeneric #-}

module Control.Monad.Class.MonadTime
  ( MonadTime (..)
  , MonadMonotonicTime (..)
    -- * 'NominalTime' and its action on 'UTCTime'
  , UTCTime
  , diffUTCTime
  , addUTCTime
  , NominalDiffTime
  ) where

import           Control.Monad.Cont
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Time.Clock (NominalDiffTime, UTCTime,
                     addUTCTime, diffUTCTime)
import qualified Data.Time.Clock as Time
import           Data.Word (Word64)
import qualified GHC.Clock as IO (getMonotonicTimeNSec)


class Monad m => MonadMonotonicTime m where
  -- | Time in a monotonic clock, with high precision. The epoch for this
  -- clock is arbitrary and does not correspond to any wall clock or calendar.
  --
  -- The time is measured in nano seconds as does `getMonotonicTimeNSec` from
  -- "base".
  --
  getMonotonicTimeNSec :: m Word64

class MonadMonotonicTime m => MonadTime m where
  -- | Wall clock time.
  --
  getCurrentTime   :: m UTCTime

--
-- Instances for IO
--

instance MonadMonotonicTime IO where
  getMonotonicTimeNSec = IO.getMonotonicTimeNSec

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

--
-- MTL instances
--

instance MonadMonotonicTime m => MonadMonotonicTime (ReaderT r m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTime m => MonadMonotonicTime (StateT s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (WriterT w m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTime m) => MonadMonotonicTime (RWST r w s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTime m => MonadMonotonicTime (ContT r m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadTime m => MonadTime (ReaderT r m) where
  getCurrentTime   = lift getCurrentTime

instance MonadTime m => MonadTime (StateT s m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (WriterT w m) where
  getCurrentTime = lift getCurrentTime

instance (Monoid w, MonadTime m) => MonadTime (RWST r w s m) where
  getCurrentTime = lift getCurrentTime

instance MonadTime m => MonadTime (ContT r m) where
  getCurrentTime   = lift getCurrentTime
