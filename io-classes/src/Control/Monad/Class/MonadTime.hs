{-# LANGUAGE DeriveGeneric #-}

module Control.Monad.Class.MonadTime
  ( MonadTime (..)
  , MonadMonotonicTimeNSec (..)
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


class Monad m => MonadMonotonicTimeNSec m where
  -- | Time in a monotonic clock, with high precision. The epoch for this
  -- clock is arbitrary and does not correspond to any wall clock or calendar.
  --
  -- The time is measured in nano seconds as does `getMonotonicTimeNSec` from
  -- "base".
  --
  getMonotonicTimeNSec :: m Word64

class MonadMonotonicTimeNSec m => MonadTime m where
  -- | Wall clock time.
  --
  getCurrentTime   :: m UTCTime

--
-- Instances for IO
--

instance MonadMonotonicTimeNSec IO where
  getMonotonicTimeNSec = IO.getMonotonicTimeNSec

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

--
-- MTL instances
--

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ReaderT r m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (StateT s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (WriterT w m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance (Monoid w, MonadMonotonicTimeNSec m) => MonadMonotonicTimeNSec (RWST r w s m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ContT r m) where
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
