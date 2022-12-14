{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}

module Control.Monad.Class.MonadTime.SI
  ( MonadTime (..)
  , MonadMonotonicTime (..)
    -- * 'DiffTime' and its action on 'Time'
  , Time (..)
  , diffTime
  , addTime
  , DiffTime
    -- * 'NominalTime' and its action on 'UTCTime'
  , UTCTime
  , diffUTCTime
  , addUTCTime
  , NominalDiffTime
  ) where

import           Control.Monad.Reader

import           Control.Monad.Class.MonadTime ( MonadMonotonicTimeNSec,
                     MonadTime (..), NominalDiffTime, UTCTime, diffUTCTime,
                     addUTCTime)
import qualified Control.Monad.Class.MonadTime as MonadTime

import           Data.Word (Word64)
import           Data.Time.Clock (DiffTime)
import qualified Data.Time.Clock as Time
import           GHC.Generics (Generic (..))


-- | A point in time in a monotonic clock.
--
-- The epoch for this clock is arbitrary and does not correspond to any wall
-- clock or calendar, and is /not guaranteed/ to be the same epoch across
-- program runs. It is represented as the 'DiffTime' from this arbitrary epoch.
--
newtype Time = Time DiffTime
  deriving (Eq, Ord, Show, Generic)

-- | The time duration between two points in time (positive or negative).
diffTime :: Time -> Time -> DiffTime
diffTime (Time t) (Time t') = t - t'

-- | Add a duration to a point in time, giving another time.
addTime :: DiffTime -> Time -> Time
addTime d (Time t) = Time (d + t)

infixr 9 `addTime`

class MonadMonotonicTimeNSec m => MonadMonotonicTime m where
  getMonotonicTime :: m Time

  default getMonotonicTime :: m Time
  getMonotonicTime =
        conv <$> MonadTime.getMonotonicTimeNSec
      where
        conv :: Word64 -> Time
        conv = Time . Time.picosecondsToDiffTime . (* 1_000) . toInteger

instance MonadMonotonicTime IO where

--
-- MTL instances
--

instance MonadMonotonicTime m => MonadMonotonicTime (ReaderT r m) where
  getMonotonicTime = lift getMonotonicTime
