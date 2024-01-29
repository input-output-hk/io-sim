{-# LANGUAGE DeriveGeneric #-}

-- | <https://hackage.haskell.org/package/time time> and
-- <https://hackage.haskell.org/package/base base> time API compatible with both
-- 'IO' and <https://hackage.haskell.org/package/io-sim IOSim>.
--
module Control.Monad.Class.MonadTime
  ( MonadTime (..)
  , MonadMonotonicTimeNSec (..)
    -- * 'NominalTime' and its action on 'UTCTime'
  , UTCTime
  , diffUTCTime
  , addUTCTime
  , NominalDiffTime
  ) where

import Control.Monad.Reader

import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock qualified as Time
import Data.Word (Word64)
import GHC.Clock qualified as IO (getMonotonicTimeNSec)


class Monad m => MonadMonotonicTimeNSec m where
  -- | Time in a monotonic clock, with high precision. The epoch for this
  -- clock is arbitrary and does not correspond to any wall clock or calendar.
  --
  -- The time is measured in nano seconds as does `getMonotonicTimeNSec` from
  -- "base".
  --
  getMonotonicTimeNSec :: m Word64

class Monad m => MonadTime m where
  -- | Wall clock time.
  --
  getCurrentTime :: m UTCTime

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

instance MonadTime m => MonadTime (ReaderT r m) where
  getCurrentTime   = lift getCurrentTime
