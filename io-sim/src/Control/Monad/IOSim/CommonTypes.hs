{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Common types shared between `IOSim` and `IOSimPOR`.
--
module Control.Monad.IOSim.CommonTypes
  ( IOSimThreadId (..)
  , ppIOSimThreadId
  , StepId
  , ppStepId
  , childThreadId
  , setRacyThread
  , TVarId (..)
  , TimeoutId (..)
  , ClockId (..)
  , VectorClock (..)
  , ppVectorClock
  , unTimeoutId
  , ThreadLabel
  , TVarLabel
  , TVar (..)
  , SomeTVar (..)
  , Deschedule (..)
  , ThreadStatus (..)
  , BlockedReason (..)
    -- * Utils
  , ppList
  ) where

import           Control.DeepSeq (NFData (..))
import           Control.Monad.Class.MonadSTM (TraceValue)
import           Control.Monad.ST.Lazy

import           NoThunks.Class

import           Data.List (intercalate, intersperse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.STRef.Lazy
import           Data.Set (Set)
import           GHC.Generics
import           Quiet


-- | A thread id.
--
-- /IOSimPOR/: 'RacyThreadId' indicates that this thread is taken into account
-- when discovering races.  A thread is marked as racy iff
-- `Control.Monad.Class.MonadTest.exploreRaces` was
-- executed in it or it's a thread forked by a racy thread.
--
data IOSimThreadId =
    -- | A racy thread (`IOSimPOR` only), shown in the trace with curly braces,
    -- e.g. `Thread {2,3}`.
    RacyThreadId ![Int]
    -- | A non racy thread.  They have higher priority than racy threads in
    -- `IOSimPOR` scheduler.
  | ThreadId     ![Int]
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving anyclass NoThunks

ppIOSimThreadId :: IOSimThreadId -> String
ppIOSimThreadId (RacyThreadId as) = "Thread {"++ intercalate "," (map show as) ++"}"
ppIOSimThreadId     (ThreadId as) = "Thread " ++ show as

childThreadId :: IOSimThreadId -> Int -> IOSimThreadId
childThreadId (RacyThreadId is) i = RacyThreadId (is ++ [i])
childThreadId (ThreadId     is) i = ThreadId     (is ++ [i])

setRacyThread :: IOSimThreadId -> IOSimThreadId
setRacyThread (ThreadId is)      = RacyThreadId is
setRacyThread tid@RacyThreadId{} = tid

-- | Execution step in `IOSimPOR` is identified by the thread id and
-- a monotonically increasing number (thread specific).
--
type StepId = (IOSimThreadId, Int)

ppStepId :: (IOSimThreadId, Int) -> String
ppStepId (tid, step) | step < 0
                     = concat [ppIOSimThreadId tid, ".-"]
ppStepId (tid, step) = concat [ppIOSimThreadId tid, ".", show step]


newtype TVarId      = TVarId    Int   deriving (Eq, Ord, Enum, Show)
newtype TimeoutId   = TimeoutId Int   deriving (Eq, Ord, Enum, Show)
newtype ClockId     = ClockId   [Int] deriving (Eq, Ord, Show)
newtype VectorClock = VectorClock { getVectorClock :: Map IOSimThreadId Int }
  deriving Generic
  deriving Show via Quiet VectorClock

ppVectorClock :: VectorClock -> String
ppVectorClock (VectorClock m) = "VectorClock " ++ "[" ++ concat (intersperse ", " (ppStepId <$> Map.toList m)) ++ "]"

unTimeoutId :: TimeoutId -> Int
unTimeoutId (TimeoutId a) = a

type ThreadLabel = String
type TVarLabel   = String

data TVar s a = TVar {

       -- | The identifier of this var.
       --
       tvarId      :: !TVarId,

       -- | Label.
       tvarLabel   :: !(STRef s (Maybe TVarLabel)),

       -- | The var's current value
       --
       tvarCurrent :: !(STRef s a),

       -- | A stack of undo values. This is only used while executing a
       -- transaction.
       --
       tvarUndo    :: !(STRef s [a]),

       -- | Thread Ids of threads blocked on a read of this var. It is
       -- represented in reverse order of thread wakeup, without duplicates.
       --
       -- To avoid duplicates efficiently, the operations rely on a copy of the
       -- thread Ids represented as a set.
       --
       tvarBlocked :: !(STRef s ([IOSimThreadId], Set IOSimThreadId)),

       -- | The vector clock of the current value.
       --
       tvarVClock  :: !(STRef s VectorClock),

       -- | Callback to construct a trace which will be attached to the dynamic
       -- trace.
       tvarTrace   :: !(STRef s (Maybe (Maybe a -> a -> ST s TraceValue)))
     }

instance Eq (TVar s a) where
    TVar {tvarId = a} == TVar {tvarId = b} = a == b

data SomeTVar s where
  SomeTVar :: !(TVar s a) -> SomeTVar s

data Deschedule = Yield
                | Interruptable
                | Blocked !BlockedReason
                | Terminated
                | Sleep
  deriving Show

data ThreadStatus = ThreadRunning
                  | ThreadBlocked !BlockedReason
                  | ThreadDone
  deriving (Eq, Show)

data BlockedReason = BlockedOnSTM
                   | BlockedOnDelay
                   | BlockedOnThrowTo
  deriving (Eq, Show)

--
-- Utils
--

ppList :: (a -> String) -> [a] -> String
ppList pp as = "[" ++ concat (intersperse ", " (map pp as)) ++ "]"
