{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Common types shared between `IOSim` and `IOSimPOR`.
--
module Control.Monad.IOSim.CommonTypes
  ( IOSimThreadId (..)
  , IOSimThreadIdMap
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

import           Data.List (intercalate)
import           Data.Set (Set)
import           Data.STRef.Lazy
import           GHC.Generics
import           Quiet

import           Data.Bifunctor (first)
import           Data.IntMap.Strict (IntMap)
import           Data.TrieMap.Class
import           Data.TrieMap.ListMap

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

-- IOSimThreadId TrieMap data type
--
data IOSimThreadIdMap v
  = IOSimThreadIdMap { ioSimRacyThreadId :: !(ListMap IntMap v)
                     , ioSimThreadId     :: !(ListMap IntMap v)
                     }

instance (Show v) => Show (IOSimThreadIdMap v) where
  show m = "fromListTM " ++ show (assocsTM m)

instance Functor IOSimThreadIdMap where
  fmap :: forall a b. (a -> b) -> IOSimThreadIdMap a -> IOSimThreadIdMap b
  fmap f IOSimThreadIdMap { ioSimRacyThreadId
                          , ioSimThreadId
                          } =
    IOSimThreadIdMap { ioSimRacyThreadId = fmap f ioSimRacyThreadId
                     , ioSimThreadId     = fmap f ioSimThreadId
                     }

instance TrieMap IOSimThreadIdMap where
   type Key IOSimThreadIdMap = IOSimThreadId

   emptyTM :: forall a. IOSimThreadIdMap a
   emptyTM  = IOSimThreadIdMap { ioSimRacyThreadId = emptyTM
                               , ioSimThreadId     = emptyTM
                               }

   lookupTM :: forall b. IOSimThreadId -> IOSimThreadIdMap b -> Maybe b
   lookupTM (RacyThreadId l)
            IOSimThreadIdMap { ioSimRacyThreadId } = lookupTM l ioSimRacyThreadId
   lookupTM (ThreadId l)
            IOSimThreadIdMap { ioSimThreadId }     = lookupTM l ioSimThreadId

   alterTM :: forall b. IOSimThreadId -> (Maybe b -> Maybe b) -> IOSimThreadIdMap b -> IOSimThreadIdMap b
   alterTM  (RacyThreadId l) f ioSimThreadIdMap@IOSimThreadIdMap { ioSimRacyThreadId } =
     ioSimThreadIdMap { ioSimRacyThreadId = alterTM l f ioSimRacyThreadId }
   alterTM  (ThreadId l)     f ioSimThreadIdMap@IOSimThreadIdMap { ioSimThreadId }     =
     ioSimThreadIdMap { ioSimThreadId = alterTM l f ioSimThreadId }

   foldTM :: forall a b. (a -> b -> b) -> IOSimThreadIdMap a -> b -> b
   foldTM f IOSimThreadIdMap
              { ioSimRacyThreadId
              , ioSimThreadId
              } = foldTM f ioSimRacyThreadId
                . foldTM f ioSimThreadId

   filterTM :: forall a. (a -> Bool) -> IOSimThreadIdMap a -> IOSimThreadIdMap a
   filterTM p IOSimThreadIdMap
                { ioSimRacyThreadId
                , ioSimThreadId
                } =
     IOSimThreadIdMap { ioSimRacyThreadId = filterTM p ioSimRacyThreadId
                      , ioSimThreadId     = filterTM p ioSimThreadId
                      }

   unionWithTM :: (a -> a -> a) -> IOSimThreadIdMap a -> IOSimThreadIdMap a -> IOSimThreadIdMap a
   unionWithTM f IOSimThreadIdMap { ioSimRacyThreadId = rtid1
                                  , ioSimThreadId     = tid1
                                  }
                 IOSimThreadIdMap { ioSimRacyThreadId = rtid2
                                  , ioSimThreadId     = tid2
                                  } =
     IOSimThreadIdMap { ioSimRacyThreadId = unionWithTM f rtid1 rtid2
                      , ioSimThreadId     = unionWithTM f tid1 tid2
                      }

   assocsTM :: IOSimThreadIdMap a -> [(IOSimThreadId, a)]
   assocsTM IOSimThreadIdMap { ioSimRacyThreadId
                             , ioSimThreadId
                             } =
       fmap (first ThreadId)     (assocsTM ioSimThreadId)
    ++ fmap (first RacyThreadId) (assocsTM ioSimRacyThreadId)

instance Foldable IOSimThreadIdMap where
  foldMap :: forall m a. Monoid m => (a -> m) -> IOSimThreadIdMap a -> m
  foldMap = foldMapTM

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
newtype VectorClock = VectorClock { getVectorClock :: IOSimThreadIdMap Int }
  deriving Generic
  deriving Show via Quiet VectorClock

ppVectorClock :: VectorClock -> String
ppVectorClock (VectorClock m) = "VectorClock " ++ "[" ++ intercalate ", " (ppStepId <$> assocsTM m) ++ "]"

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
ppList pp as = "[" ++ intercalate ", " (map pp as) ++ "]"
