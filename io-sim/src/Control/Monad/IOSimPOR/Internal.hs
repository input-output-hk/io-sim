{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
-- incomplete uni patterns in 'schedule' (when interpreting 'StmTxCommitted')
-- and 'reschedule'.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-matches #-}

module Control.Monad.IOSimPOR.Internal
  ( IOSim (..)
  , SimM
  , runIOSim
  , runSimTraceST
  , traceM
  , traceSTM
  , STM
  , STMSim
  , SimSTM
  , setCurrentTime
  , unshareClock
  , TimeoutException (..)
  , EventlogEvent (..)
  , EventlogMarker (..)
  , ThreadId
  , ThreadLabel
  , Labelled (..)
  , SimTrace
  , Trace.Trace (SimPORTrace, Trace, TraceMainReturn, TraceMainException, TraceDeadlock)
  , SimEvent (..)
  , SimResult (..)
  , SimEventType (..)
  , TraceEvent
  , liftST
  , execReadTVar
  , controlSimTraceST
  , ScheduleControl (..)
  , ScheduleMod (..)
  ) where

import           Prelude hiding (read)

import           Data.Dynamic
import           Data.Foldable (foldlM, traverse_)
import qualified Data.List as List
import qualified Data.List.Trace as Trace
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Ord
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)

import           Control.Exception (AsyncException (..), NonTermination (..),
                     assert, throw)
import           Control.Monad (join, when)
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Lazy.Unsafe (unsafeIOToST, unsafeInterleaveST)
import           Data.STRef.Lazy

import           Control.Concurrent.Class.MonadSTM.TVar hiding (TVar)
import           Control.Monad.Class.MonadSTM hiding (STM)
import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Monad.IOSim.InternalTypes
import           Control.Monad.IOSim.Types hiding (SimEvent (SimEvent),
                     Trace (SimTrace))
import           Control.Monad.IOSim.Types (SimEvent)
import           Control.Monad.IOSimPOR.Timeout (unsafeTimeout)
import           Control.Monad.IOSimPOR.Types

import           GHC.Conc (ThreadStatus(..), BlockReason(..))

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !ThreadId,
    threadControl :: !(ThreadControl s a),
    threadBlocked :: !Bool,
    threadDone    :: !Bool,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, Labelled ThreadId, VectorClock)],
    threadClockId :: !ClockId,
    threadLabel   :: Maybe ThreadLabel,
    threadNextTId :: !Int,
    threadStep    :: !Int,
    threadVClock  :: VectorClock,
    threadEffect  :: Effect,  -- in the current step
    threadRacy    :: !Bool
  }
  deriving Show

threadStepId :: Thread s a -> (ThreadId, Int)
threadStepId Thread{ threadId, threadStep } = (threadId, threadStep)

isRacyThreadId :: ThreadId -> Bool
isRacyThreadId (RacyThreadId _) = True
isRacyThreadId _                = True

isNotRacyThreadId :: ThreadId -> Bool
isNotRacyThreadId (ThreadId _) = True
isNotRacyThreadId _            = False

bottomVClock :: VectorClock
bottomVClock = VectorClock Map.empty

insertVClock :: ThreadId -> Int -> VectorClock -> VectorClock
insertVClock tid !step (VectorClock m) = VectorClock (Map.insert tid step m)

leastUpperBoundVClock :: VectorClock -> VectorClock -> VectorClock
leastUpperBoundVClock (VectorClock m) (VectorClock m') =
    VectorClock (Map.unionWith max m m')

-- hbfVClock :: VectorClock -> VectorClock -> Bool
-- hbfVClock (VectorClock m) (VectorClock m') = Map.isSubmapOfBy (<=) m m'

happensBeforeStep :: Step -- ^ an earlier step
                  -> Step -- ^ a later step
                  -> Bool
happensBeforeStep step step' =
       Just (stepStep step)
    <= Map.lookup (stepThreadId step)
                  (getVectorClock $ stepVClock step')

labelledTVarId :: TVar s a -> ST s (Labelled TVarId)
labelledTVarId TVar { tvarId, tvarLabel } = Labelled tvarId <$> readSTRef tvarLabel

labelledThreads :: Map ThreadId (Thread s a) -> [Labelled ThreadId]
labelledThreads threadMap =
    -- @Map.foldr'@ (and alikes) are not strict enough, to not retain the
    -- original thread map we need to evaluate the spine of the list.
    -- TODO: https://github.com/haskell/containers/issues/749
    Map.foldr'
      (\Thread { threadId, threadLabel } !acc -> Labelled threadId threadLabel : acc)
      [] threadMap


-- | Timers mutable variables.  First one supports 'newTimeout' api, the second
-- one 'registerDelay', the third one 'threadDelay'.
--
data TimerCompletionInfo s =
       Timer !(TVar s TimeoutState)
     | TimerRegisterDelay !(TVar s Bool)
     | TimerThreadDelay !ThreadId
     | TimerTimeout !ThreadId !TimeoutId !(STRef s IsLocked)

type RunQueue = OrdPSQ (Down ThreadId) (Down ThreadId) ()

-- | Internal state.
--
data SimState s a = SimState {
       runqueue         :: !RunQueue,
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads          :: !(Map ThreadId (Thread s a)),
       -- | All finished threads (needed for threadStatus)
       finished         :: !(Map ThreadId (FinishedReason, VectorClock)),
       -- | current time
       curTime          :: !Time,
       -- | ordered list of timers and timeouts
       timers           :: !(OrdPSQ TimeoutId Time (TimerCompletionInfo s)),
       -- | timeout locks in order to synchronize the timeout handler and the
       -- main thread
       clocks           :: !(Map ClockId UTCTime),
       nextVid          :: !TVarId,     -- ^ next unused 'TVarId'
       nextTmid         :: !TimeoutId,  -- ^ next unused 'TimeoutId'
       -- | previous steps (which we may race with).
       -- Note this is *lazy*, so that we don't compute races we will not reverse.
       races            :: Races,
       -- | control the schedule followed, and initial value
       control          :: !ScheduleControl,
       control0         :: !ScheduleControl,
       -- | limit on the computation time allowed per scheduling step, for
       -- catching infinite loops etc
       perStepTimeLimit :: Maybe Int

     }

initialState :: SimState s a
initialState =
    SimState {
      runqueue = PSQ.empty,
      threads  = Map.empty,
      finished = Map.empty,
      curTime  = Time 0,
      timers   = PSQ.empty,
      clocks   = Map.singleton (ClockId []) epoch1970,
      nextVid  = TVarId 0,
      nextTmid = TimeoutId 0,
      races    = noRaces,
      control  = ControlDefault,
      control0 = ControlDefault,
      perStepTimeLimit = Nothing
    }
  where
    epoch1970 = UTCTime (fromGregorian 1970 1 1) 0

invariant :: Maybe (Thread s a) -> SimState s a -> x -> x

invariant (Just running) simstate@SimState{runqueue,threads,finished,clocks} =
    assert (not (threadBlocked running))
  . assert (threadId running `Map.notMember` finished)
  . assert (threadId running `Map.notMember` threads)
  . assert (not (Down (threadId running) `PSQ.member` runqueue))
  . assert (threadClockId running `Map.member` clocks)
  . invariant Nothing simstate

invariant Nothing SimState{runqueue,threads,finished,clocks} =
    assert (PSQ.fold' (\(Down tid) _ _ a -> tid `Map.member` threads && a) True runqueue)
  . assert (and [ (threadBlocked t || threadDone t) == not (Down (threadId t) `PSQ.member` runqueue)
                | t <- Map.elems threads ])
  . assert (and (zipWith (\(Down tid, _, _) (Down tid', _, _) -> tid > tid')
                         (PSQ.toList runqueue)
                         (drop 1 (PSQ.toList runqueue))))
  . assert (and [ threadClockId t `Map.member` clocks
                | t <- Map.elems threads ])
  . assert (Map.keysSet finished == Map.keysSet (Map.filter threadDone threads))

-- | Interpret the simulation monotonic time as a 'NominalDiffTime' since
-- the start.
timeSinceEpoch :: Time -> NominalDiffTime
timeSinceEpoch (Time t) = fromRational (toRational t)


-- | Insert thread into `runqueue`.
--
insertThread :: Thread s a -> RunQueue -> RunQueue
insertThread Thread { threadId } = PSQ.insert (Down threadId) (Down threadId) ()


-- | Schedule / run a thread.
--
schedule :: Thread s a -> SimState s a -> ST s (SimTrace a)
schedule thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl,
           threadMasking = maskst,
           threadLabel   = tlbl,
           threadStep    = tstep,
           threadVClock  = vClock,
           threadEffect  = effect
         }
         simstate@SimState {
           runqueue,
           threads,
           finished,
           timers,
           clocks,
           nextVid, nextTmid,
           curTime  = time,
           control,
           perStepTimeLimit
         }

  | controlTargets (tid,tstep) control =
      -- The next step is to be delayed according to the
      -- specified schedule. Switch to following the schedule.
      SimPORTrace time tid tstep tlbl (EventFollowControl control) <$>
      schedule thread simstate{ control = followControl control }

  | not $ controlFollows (tid,tstep) control =
      -- the control says this is not the next step to
      -- follow. We should be at the beginning of a step;
      -- we put the present thread to sleep and reschedule
      -- the correct thread.
      -- The assertion says that the only effect that may have
      -- happened in the start of a thread is us waking up.
      assert (effect { effectStatusWrites = filter (/= tid) (effectStatusWrites effect) } == mempty) $
      ( SimPORTrace time tid tstep tlbl (EventAwaitControl (tid,tstep) control)
      . SimPORTrace time tid tstep tlbl (EventDeschedule Sleep)
      ) <$> deschedule Sleep thread simstate

  | otherwise =
  invariant (Just thread) simstate $
  case control of
    ControlFollow (s:_) _
      -> fmap (SimPORTrace time tid tstep tlbl (EventPerformAction (tid,tstep)))
    _ -> id
  $
  -- The next line forces the evaluation of action, which should be unevaluated up to
  -- this point. This is where we actually *run* user code.
  case maybe Just unsafeTimeout perStepTimeLimit action of
   Nothing -> return TraceLoop
   Just _  -> case action of

    Return x -> case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ SimPORTrace time tid tstep tlbl EventThreadFinished
               $ traceFinalRacesFound simstate
               $ TraceMainReturn time x ( labelledThreads
                                        . Map.filter (not . threadDone)
                                        $ threads
                                        )

      ForkFrame -> do
        -- this thread is done
        let thread' = thread
        !trace <- deschedule (Terminated FinishedNormally) thread' simstate
        return $ SimPORTrace time tid tstep tlbl EventThreadFinished
               $ SimPORTrace time tid tstep tlbl (EventDeschedule $ Terminated FinishedNormally)
               $ trace

      MaskFrame k maskst' ctl' -> do
        -- pop the control stack, restore thread-local state
        let thread' = thread { threadControl = ThreadControl (k x) ctl'
                             , threadMasking = maskst'
                             }
        -- but if we're now unmasked, check for any pending async exceptions
        !trace <- deschedule Interruptable thread' simstate
        return $ SimPORTrace time tid tstep tlbl (EventMask maskst')
               $ SimPORTrace time tid tstep tlbl (EventDeschedule Interruptable)
               $ trace

      CatchFrame _handler k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

      TimeoutFrame tmid isLockedRef k ctl' -> do
        -- It could happen that the timeout action finished at the same time
        -- as the timeout expired, this will be a race condition. That's why
        -- we have the locks to solve this.
        --
        -- The lock starts 'NotLocked' and when the timeout fires the lock is
        -- locked and asynchronously an assassin thread is coming to interrupt
        -- this one. If the lock is locked when the timeout is fired then nothing
        -- happens.
        --
        -- Knowing this, if we reached this point in the code and the lock is
        -- 'Locked', then it means that this thread still hasn't received the
        -- 'TimeoutException', so we need to kill the thread that is responsible
        -- for doing that (the assassin one, we need to defend ourselves!)
        -- and run our continuation successfully and peacefully. We will do that
        -- by uninterruptibly-masking ourselves so we can not receive any
        -- exception and kill the assassin thread behind its back.
        -- If the lock is 'NotLocked' then it means we can just acquire it and
        -- carry on with the success case.
        locked <- readSTRef isLockedRef
        case locked of
          Locked etid -> do
            let -- Kill the exception throwing thread and carry on the
                -- continuation
                thread' =
                  thread { threadControl =
                            ThreadControl (ThrowTo (toException ThreadKilled)
                                                   etid
                                                   (k (Just x)))
                                          ctl'
                         , threadMasking = MaskedUninterruptible
                         }
            schedule thread' simstate

          NotLocked -> do
            -- Acquire lock
            writeSTRef isLockedRef (Locked tid)

                -- Remove the timer from the queue
            let timers' = PSQ.delete tmid timers
                -- Run the continuation successfully
                thread' = thread { threadControl = ThreadControl (k (Just x)) ctl' }

            schedule thread' simstate { timers = timers'
                                      }
    Throw thrower e -> case unwindControlStack e thread of
      -- Found a CatchFrame
      Right thread0@Thread { threadMasking = maskst' } -> do
        -- We found a suitable exception handler, continue with that
        -- We record a step, in case there is no exception handler on replay.
        let thread'  = stepThread thread0
            control' = advanceControl (threadStepId thread0) control
            races'   = updateRacesInSimState thread0 simstate
        trace <- schedule thread' simstate{ races = races', control = control' }
        return (SimPORTrace time tid tstep tlbl (EventThrow e) $
                SimPORTrace time tid tstep tlbl (EventMask maskst') trace)

      Left isMain
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain -> do
          let thread' = thread { threadEffect = effect <> statusWriteEffect tid }
          -- An unhandled exception in the main thread terminates the program
          return (SimPORTrace time tid tstep tlbl (EventThrow e) $
                  SimPORTrace time tid tstep tlbl (EventThreadUnhandled e) $
                  traceFinalRacesFound simstate { threads = Map.insert tid thread' threads } $
                  TraceMainException time e (labelledThreads threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          let reason = if thrower == ThrowSelf then FinishedNormally else FinishedDied
              thread' = thread { threadEffect  = effect <> statusWriteEffect tid
                               }
              terminated = Terminated reason
          !trace <- deschedule terminated thread' simstate
          return $ SimPORTrace time tid tstep tlbl (EventThrow e)
                 $ SimPORTrace time tid tstep tlbl (EventThreadUnhandled e)
                 $ SimPORTrace time tid tstep tlbl (EventDeschedule terminated)
                 $ trace

    Catch action' handler k -> do
      -- push the failure and success continuations onto the control stack
      let thread' = thread { threadControl = ThreadControl action'
                                               (CatchFrame handler k ctl)
                           }
      schedule thread' simstate

    Evaluate expr k -> do
      mbWHNF <- unsafeIOToST $ try $ evaluate expr
      case mbWHNF of
        Left e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw ThrowSelf e) ctl }
          schedule thread' simstate
        Right whnf -> do
          -- continue with the resulting WHNF
          let thread' = thread { threadControl = ThreadControl (k whnf) ctl }
          schedule thread' simstate

    Say msg k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimPORTrace time tid tstep tlbl (EventSay msg) trace)

    Output x k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimPORTrace time tid tstep tlbl (EventLog x) trace)

    LiftST st k -> do
      x <- strictToLazyST st
      let thread' = thread { threadControl = ThreadControl (k x) ctl }
      schedule thread' simstate

    GetMonoTime k -> do
      let thread' = thread { threadControl = ThreadControl (k time) ctl }
      schedule thread' simstate

    GetWallTime k -> do
      let clockid  = threadClockId thread
          clockoff = clocks Map.! clockid
          walltime = timeSinceEpoch time `addUTCTime` clockoff
          thread'  = thread { threadControl = ThreadControl (k walltime) ctl }
      schedule thread' simstate

    SetWallTime walltime' k -> do
      let clockid   = threadClockId thread
          clockoff  = clocks Map.! clockid
          walltime  = timeSinceEpoch time `addUTCTime` clockoff
          clockoff' = addUTCTime (diffUTCTime walltime' walltime) clockoff
          thread'   = thread { threadControl = ThreadControl k ctl }
          simstate' = simstate { clocks = Map.insert clockid clockoff' clocks }
      schedule thread' simstate'

    UnshareClock k -> do
      let clockid   = threadClockId thread
          clockoff  = clocks Map.! clockid
          clockid'  = let ThreadId i = tid in ClockId i -- reuse the thread id
          thread'   = thread { threadControl = ThreadControl k ctl
                             , threadClockId = clockid' }
          simstate' = simstate { clocks = Map.insert clockid' clockoff clocks }
      schedule thread' simstate'

    -- we treat negative timers as cancelled ones; for the record we put
    -- `EventTimerCreated` and `EventTimerCancelled` in the trace; This differs
    -- from `GHC.Event` behaviour.
    NewTimeout d k | d < 0 -> do
      let t       = NegativeTimeout nextTmid
          expiry  = d `addTime` time
          thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { nextTmid = succ nextTmid }
      return (SimPORTrace time tid tstep tlbl (EventTimerCreated nextTmid nextVid expiry) $
              SimPORTrace time tid tstep tlbl (EventTimerCancelled nextTmid) $
              trace)

    NewTimeout d k -> do
      tvar  <- execNewTVar nextVid
                           (Just $ "<<timeout-state " ++ show (unTimeoutId nextTmid) ++ ">>")
                           TimeoutPending
      modifySTRef (tvarVClock tvar) (leastUpperBoundVClock vClock)
      let expiry  = d `addTime` time
          t       = Timeout tvar nextTmid
          timers' = PSQ.insert nextTmid expiry (Timer tvar) timers
          thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                          , nextVid  = succ (succ nextVid)
                                          , nextTmid = succ nextTmid }
      return (SimPORTrace time tid tstep tlbl (EventTimerCreated nextTmid nextVid expiry) trace)

    -- This case is guarded by checks in 'timeout' itself.
    StartTimeout d _ _ | d <= 0 ->
      error "schedule: StartTimeout: Impossible happened"

    StartTimeout d action' k -> do
      isLockedRef <- newSTRef NotLocked
      let expiry    = d `addTime` time
          timers'   = PSQ.insert nextTmid expiry (TimerTimeout tid nextTmid isLockedRef) timers
          thread'   = thread { threadControl =
                                 ThreadControl action'
                                               (TimeoutFrame nextTmid isLockedRef k ctl)
                              }
      trace <- deschedule Yield thread' simstate { timers   = timers'
                                                  , nextTmid = succ nextTmid }
      return (SimPORTrace time tid tstep tlbl (EventTimeoutCreated nextTmid tid expiry) trace)

    RegisterDelay d k | d < 0 -> do
      tvar <- execNewTVar nextVid
                          (Just $ "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                          True
      modifySTRef (tvarVClock tvar) (leastUpperBoundVClock vClock)
      let !expiry  = d `addTime` time
          !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
      trace <- schedule thread' simstate { nextVid = succ nextVid }
      return (SimPORTrace time tid tstep tlbl (EventRegisterDelayCreated nextTmid nextVid expiry) $
              SimPORTrace time tid tstep tlbl (EventRegisterDelayFired nextTmid) $
              trace)

    RegisterDelay d k -> do
      tvar <- execNewTVar nextVid
                          (Just $ "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                          False
      modifySTRef (tvarVClock tvar) (leastUpperBoundVClock vClock)
      let !expiry  = d `addTime` time
          !timers' = PSQ.insert nextTmid expiry (TimerRegisterDelay tvar) timers
          !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (SimPORTrace time tid tstep tlbl
                (EventRegisterDelayCreated nextTmid nextVid expiry) trace)

    ThreadDelay d k | d < 0 -> do
      let expiry  = d `addTime` time
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimPORTrace time tid tstep tlbl (EventThreadDelay expiry) $
              SimPORTrace time tid tstep tlbl EventThreadDelayFired $
              trace)

    ThreadDelay d k -> do
      let expiry  = d `addTime` time
          timers' = PSQ.insert nextTmid expiry (TimerThreadDelay tid) timers
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- deschedule Blocked thread' simstate { timers   = timers'
                                                    , nextTmid = succ nextTmid }
      return (SimPORTrace time tid tstep tlbl (EventThreadDelay expiry) trace)

    -- we do not follow `GHC.Event` behaviour here; updating a timer to the past
    -- effectively cancels it.
    UpdateTimeout (Timeout _tvar tmid) d k | d < 0 -> do
      let timers' = PSQ.delete tmid timers
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimPORTrace time tid tstep tlbl (EventTimerCancelled tmid) trace)

    UpdateTimeout (Timeout _tvar tmid) d k -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimeout_  Nothing       = ((), Nothing)
          updateTimeout_ (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimeout_ tmid timers)
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimPORTrace time tid tstep tlbl (EventTimerUpdated tmid expiry) trace)

    -- updating a negative timer is a no-op, unlike in `GHC.Event`.
    UpdateTimeout (NegativeTimeout _tmid) _d k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    CancelTimeout (Timeout tvar tmid) k -> do
      let timers' = PSQ.delete tmid timers
      written <- execAtomically' (runSTM $ writeTVar tvar TimeoutCancelled)
      (wakeup, wokeby) <- threadsUnblockedByWrites written
      mapM_ (\(SomeTVar var) -> unblockAllThreadsFromTVar var) written
      let effect' = effect
                 <> writeEffects written
                 <> wakeupEffects wakeup
                 <> statusWriteEffects unblocked
          thread' = thread { threadControl = ThreadControl k ctl
                           , threadEffect  = effect'
                           }
          (unblocked,
           simstate') = unblockThreads vClock wakeup simstate
      modifySTRef (tvarVClock tvar)  (leastUpperBoundVClock vClock)
      !trace <- deschedule Yield thread' simstate' { timers = timers' }
      return $ SimPORTrace time tid tstep tlbl (EventTimerCancelled tmid)
             $ traceMany
                 -- TODO: step
                 [ (time, tid', (-1), tlbl', EventTxWakeup vids)
                 | tid' <- unblocked
                 , let tlbl' = lookupThreadLabel tid' threads
                 , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
             $ SimPORTrace time tid tstep tlbl (EventDeschedule Yield)
             $ trace

    -- cancelling a negative timer is a no-op
    CancelTimeout (NegativeTimeout _tmid) k -> do
      -- negative timers are promptly removed from the state
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    Fork a k -> do
      let nextTId = threadNextTId thread
          tid' | threadRacy thread = setRacyThread $ childThreadId tid nextTId
               | otherwise         = childThreadId tid nextTId
          thread'  = thread { threadControl = ThreadControl (k tid') ctl,
                              threadNextTId = nextTId + 1,
                              threadEffect  = effect
                                           <> forkEffect tid'
                                           <> statusWriteEffect tid'
                                           <> statusWriteEffect tid
                              }
          thread'' = Thread { threadId      = tid'
                            , threadControl = ThreadControl (runIOSim a)
                                                            ForkFrame
                            , threadBlocked = False
                            , threadDone    = False
                            , threadMasking = threadMasking thread
                            , threadThrowTo = []
                            , threadClockId = threadClockId thread
                            , threadLabel   = Nothing
                            , threadNextTId = 1
                            , threadStep    = 0
                            , threadVClock  = insertVClock tid' 0
                                            $ vClock
                            , threadEffect  = mempty
                            , threadRacy    = threadRacy thread
                            }
          threads' = Map.insert tid' thread'' threads
      -- A newly forked thread may have a higher priority, so we deschedule this one.
      !trace <- deschedule Yield thread'
                  simstate { runqueue = insertThread thread'' runqueue
                           , threads  = threads' }
      return $ SimPORTrace time tid tstep tlbl (EventThreadForked tid')
             $ SimPORTrace time tid tstep tlbl (EventDeschedule Yield)
             $ trace

    Atomically a k -> execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
      case res of
        StmTxCommitted x written read created
                         tvarDynamicTraces tvarStringTraces nextVid' -> do
          (wakeup, wokeby) <- threadsUnblockedByWrites written
          mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          vClockRead <- leastUpperBoundTVarVClocks read
          let vClock'     = vClock `leastUpperBoundVClock` vClockRead
              effect'     = effect
                         <> readEffects read
                         <> writeEffects written
                         <> wakeupEffects unblocked
                         <> statusWriteEffects unblocked
              thread'     = thread { threadControl = ThreadControl (k x) ctl,
                                     threadVClock  = vClock',
                                     threadEffect  = effect' }
              (unblocked,
               simstate') = unblockThreads vClock' wakeup simstate
          sequence_ [ modifySTRef (tvarVClock r) (leastUpperBoundVClock vClock')
                    | SomeTVar r <- created ++ written ]
          written' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
          created' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) created
          -- We deschedule a thread after a transaction... another may have woken up.
          !trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $
            SimPORTrace time tid tstep tlbl (EventTxCommitted written' created' (Just effect')) $
            traceMany
              [ (time, tid', tstep, tlbl', EventTxWakeup vids')
              | tid' <- unblocked
              , let tlbl' = lookupThreadLabel tid' threads
              , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ] $
            traceMany
              [ (time, tid, tstep, tlbl, EventLog tr)
              | tr <- tvarDynamicTraces
              ] $
            traceMany
              [ (time, tid, tstep, tlbl, EventSay str)
              | str <- tvarStringTraces
              ] $
            SimPORTrace time tid tstep tlbl (EventUnblocked unblocked) $
            SimPORTrace time tid tstep tlbl (EventDeschedule Yield) $
              trace

        StmTxAborted read e -> do
          -- schedule this thread to immediately raise the exception
          vClockRead <- leastUpperBoundTVarVClocks read
          let effect' = effect <> readEffects read
              thread' = thread { threadControl = ThreadControl (Throw ThrowSelf e) ctl,
                                 threadVClock  = vClock `leastUpperBoundVClock` vClockRead,
                                 threadEffect  = effect' }
          trace <- schedule thread' simstate
          return $ SimPORTrace time tid tstep tlbl (EventTxAborted (Just effect'))
                 $ trace

        StmTxBlocked read -> do
          mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
          vClockRead <- leastUpperBoundTVarVClocks read
          let effect' = effect <> readEffects read
              thread' = thread { threadVClock  = vClock `leastUpperBoundVClock` vClockRead,
                                 threadEffect  = effect' }
          !trace <- deschedule Blocked thread' simstate
          return $ SimPORTrace time tid tstep tlbl (EventTxBlocked vids (Just effect'))
                 $ SimPORTrace time tid tstep tlbl (EventDeschedule Blocked)
                 $ trace

    GetThreadId k -> do
      let thread' = thread { threadControl = ThreadControl (k tid) ctl }
      schedule thread' simstate

    LabelThread tid' l k | tid' == tid -> do
      let thread' = thread { threadControl = ThreadControl k ctl
                           , threadLabel   = Just l }
      schedule thread' simstate

    LabelThread tid' l k -> do
      let thread'  = thread { threadControl = ThreadControl k ctl }
          threads' = Map.adjust (\t -> t { threadLabel = Just l }) tid' threads
      schedule thread' simstate { threads = threads' }

    ThreadStatus tid' k -> do
      let result | Just (r, _) <- Map.lookup tid' finished = reasonToStatus r
                 | Just t <- Map.lookup tid' threads       = threadStatus t
                 | tid' == tid                             = ThreadRunning
                 | otherwise                               = error "The impossible happened - tried to loookup thread in state."
          otherVClock | Just t <- Map.lookup tid' threads       = threadVClock t
                      | Just (_, c) <- Map.lookup tid' finished = c
                      | tid' == tid                             = vClock
                      | otherwise                               = error "The impossible happened"
          reasonToStatus FinishedNormally  = ThreadFinished
          reasonToStatus FinishedDied      = ThreadDied
          threadStatus t | threadBlocked t = ThreadBlocked BlockedOnOther
                         | otherwise       = ThreadRunning

          thread' = thread { threadControl = ThreadControl (k result) ctl
                           , threadVClock  = vClock `leastUpperBoundVClock` otherVClock
                           , threadEffect  = effect <> statusReadEffects [tid']
                           }
      trace <- schedule thread' simstate
      return $ SimPORTrace time tid tstep tlbl (EventThreadStatus tid tid')
             $ trace

    ExploreRaces k -> do
      let thread'  = thread { threadControl = ThreadControl k ctl
                            , threadRacy    = True }
      schedule thread' simstate

    Fix f k -> do
      r <- newSTRef (throw NonTermination)
      x <- unsafeInterleaveST $ readSTRef r
      let k' = unIOSim (f x) $ \x' ->
                  LiftST (lazyToStrictST (writeSTRef r x')) (\() -> k x')
          thread' = thread { threadControl = ThreadControl k' ctl }
      schedule thread' simstate

    GetMaskState k -> do
      let thread' = thread { threadControl = ThreadControl (k maskst) ctl }
      schedule thread' simstate

    SetMaskState maskst' action' k -> do
      let thread' = thread { threadControl = ThreadControl
                                               (runIOSim action')
                                               (MaskFrame k maskst ctl)
                           , threadMasking = maskst' }
      trace <-
        case maskst' of
          -- If we're now unmasked then check for any pending async exceptions
          Unmasked -> SimPORTrace time tid tstep tlbl (EventDeschedule Interruptable)
                  <$> deschedule Interruptable thread' simstate
          _        -> schedule                 thread' simstate
      return $ SimPORTrace time tid tstep tlbl (EventMask maskst')
             $ trace

    ThrowTo e tid' _ | tid' == tid -> do
      -- Throw to ourself is equivalent to a synchronous throw,
      -- and works irrespective of masking state since it does not block.
      let thread' = thread { threadControl = ThreadControl (Throw ThrowSelf e) ctl
                           , threadEffect  = effect
                           }
      trace <- schedule thread' simstate
      return (SimPORTrace time tid tstep tlbl (EventThrowTo e tid) trace)

    ThrowTo e tid' k -> do
      let thread'    = thread { threadControl = ThreadControl k ctl,
                                threadEffect  = effect <> throwToEffect tid'
                                                       <> wakeUpEffect
                                                       <> (if willBlock
                                                           then statusWriteEffect tid
                                                           else mempty),
                                threadVClock  = vClock `leastUpperBoundVClock` vClockTgt
                              }
          (vClockTgt,
           wakeUpEffect,
           willBlock) = (threadVClock t,
                         if threadBlocked t then wakeupEffects [tid'] else mempty,
                         not (threadInterruptible t || threadDone t))
            where Just t = Map.lookup tid' threads

      if willBlock
        then do
          -- The target thread has async exceptions masked so we add the
          -- exception and the source thread id to the pending async exceptions.
          let adjustTarget t =
                t { threadThrowTo = (e, Labelled tid tlbl, vClock) : threadThrowTo t }
              threads'       = Map.adjust adjustTarget tid' threads
          trace <- deschedule Blocked thread' simstate { threads = threads' }
          return $ SimPORTrace time tid tstep tlbl (EventThrowTo e tid')
                 $ SimPORTrace time tid tstep tlbl EventThrowToBlocked
                 $ SimPORTrace time tid tstep tlbl (EventDeschedule Blocked)
                 $ trace
        else do
          -- The target thread has async exceptions unmasked, or is masked but
          -- is blocked (and all blocking operations are interruptible) then we
          -- raise the exception in that thread immediately. This will either
          -- cause it to terminate or enter an exception handler.
          -- In the meantime the thread masks new async exceptions. This will
          -- be resolved if the thread terminates or if it leaves the exception
          -- handler (when restoring the masking state would trigger the any
          -- new pending async exception).
          let adjustTarget t@Thread{ threadControl = ThreadControl _ ctl',
                                     threadVClock  = vClock' } =
                t { threadControl = ThreadControl (Throw ThrowOther e) ctl'
                  , threadBlocked = False
                  , threadVClock  = vClock' `leastUpperBoundVClock` vClock }
              (unblocked, simstate'@SimState { threads = threads' }) = unblockThreads vClock [tid'] simstate
              threads''  = Map.adjust adjustTarget tid' threads'
              simstate'' = simstate' { threads = threads'' }

          -- We yield at this point because the target thread may be higher
          -- priority, so this should be a step for race detection.
          trace <- deschedule Yield thread' { threadEffect = threadEffect thread' <> statusWriteEffects unblocked } simstate''
          return $ SimPORTrace time tid tstep tlbl (EventThrowTo e tid')
                 $ trace

    -- intentionally a no-op (at least for now)
    YieldSim k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate


threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                 -> True
      MaskedInterruptible
        | threadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise            -> False
      MaskedUninterruptible    -> False

deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (SimTrace a)

deschedule Yield thread@Thread { threadId = tid }
                 simstate@SimState{runqueue, threads, control} =

    -- We don't interrupt runnable threads anywhere else.
    -- We do it here by inserting the current thread into the runqueue in priority order.

    let thread'   = stepThread thread
        runqueue' = insertThread thread' runqueue
        threads'  = Map.insert tid thread' threads
        control'  = advanceControl (threadStepId thread) control in
    reschedule simstate { runqueue = runqueue',
                          threads  = threads',
                          races    = updateRacesInSimState thread simstate,
                          control  = control' }

deschedule Interruptable thread@Thread {
                           threadId      = tid,
                           threadStep    = tstep,
                           threadControl = ThreadControl _ ctl,
                           threadMasking = Unmasked,
                           threadThrowTo = (e, tid', vClock') : etids,
                           threadLabel   = tlbl,
                           threadVClock  = vClock,
                           threadEffect  = effect
                         }
                        simstate@SimState{ curTime = time, threads } = do

    -- We're unmasking, but there are pending blocked async exceptions.
    -- So immediately raise the exception and unblock the blocked thread
    -- if possible.
    let thread' = thread { threadControl = ThreadControl (Throw ThrowOther e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids
                         , threadVClock  = vClock `leastUpperBoundVClock` vClock'
                         , threadEffect  = effect <> statusWriteEffects unblocked
                         }
        (unblocked,
         simstate') = unblockThreads vClock [l_labelled tid'] simstate
    -- the thread is stepped when we Yield
    !trace <- deschedule Yield thread' simstate'
    return $ SimPORTrace time tid tstep tlbl (EventDeschedule Yield)
           $ SimPORTrace time tid tstep tlbl (EventThrowToUnmasked tid')
           -- TODO: step
           $ traceMany [ (time, tid'', (-1), tlbl'', EventThrowToWakeup)
                       | tid'' <- unblocked
                       , let tlbl'' = lookupThreadLabel tid'' threads ]
             trace

deschedule Interruptable thread@Thread{threadId = tid } simstate@SimState{ control } =
    -- Either masked or unmasked but no pending async exceptions.
    -- Either way, just carry on.
    -- Record a step, though, in case on replay there is an async exception.
    let thread' = stepThread thread in
    schedule thread'
             simstate{ races   = updateRacesInSimState thread simstate,
                       control = advanceControl (threadStepId thread) control }

deschedule Blocked thread@Thread { threadId      = tid
                                 , threadThrowTo = _ : _
                                 , threadMasking = maskst
                                 , threadEffect  = effect } simstate
    | maskst /= MaskedUninterruptible =
    -- We're doing a blocking operation, which is an interrupt point even if
    -- we have async exceptions masked, and there are pending blocked async
    -- exceptions. So immediately raise the exception and unblock the blocked
    -- thread if possible.
    deschedule Interruptable thread { threadMasking = Unmasked, threadEffect = effect <> statusWriteEffect tid } simstate

deschedule Blocked thread@Thread{ threadId = tid, threadEffect = effect } simstate@SimState{threads, control} =
    let thread1 = thread { threadBlocked = True , threadEffect = effect <> statusWriteEffect tid }
        thread'  = stepThread thread1
        threads' = Map.insert (threadId thread') thread' threads in
    reschedule simstate { threads = threads',
                          races   = updateRacesInSimState thread1 simstate,
                          control = advanceControl (threadStepId thread1) control }

deschedule (Terminated reason) thread@Thread { threadId = tid, threadVClock = vClock, threadEffect = effect }
                               simstate@SimState{ curTime = time, control, finished = finished } = do
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    let thread1     = thread { threadEffect = effect <> statusWriteEffect tid }
        thread'     = stepThread $ thread { threadDone = True }
        wakeup      = map (\(_,tid',_) -> l_labelled tid') (reverse (threadThrowTo thread))
        (unblocked,
         simstate'@SimState{threads}) =
                      unblockThreads vClock wakeup simstate
        threads'    = Map.insert tid thread' threads
    -- We must keep terminated threads in the state to preserve their vector clocks,
    -- which matters when other threads throwTo them.
    !trace <- reschedule simstate' { races = threadTerminatesRaces tid $
                                              updateRacesInSimState thread1 simstate,
                                    control = advanceControl (threadStepId thread) control,
                                    threads = threads',
                                    finished = Map.insert tid (reason, vClock) finished }
    return $ traceMany
               -- TODO: step
               [ (time, tid', (-1), tlbl', EventThrowToWakeup)
               | tid' <- unblocked
               , let tlbl' = lookupThreadLabel tid' threads ]
               trace

deschedule Sleep thread@Thread { threadId = tid , threadEffect = effect }
                 simstate@SimState{runqueue, threads} =

    -- Schedule control says we should run a different thread. Put
    -- this one to sleep without recording a step.

    let thread' = thread { threadEffect = effect <> statusWriteEffect tid }
        runqueue' = insertThread thread runqueue
        threads'  = Map.insert tid thread' threads in
    reschedule simstate { runqueue = runqueue', threads  = threads' }


-- Choose the next thread to run.
reschedule :: SimState s a -> ST s (SimTrace a)

-- If we are following a controlled schedule, just do that.
reschedule simstate@SimState{ runqueue, threads,
                              control=control@(ControlFollow ((tid,tstep):_) _),
                              curTime=time
                              } =
    fmap (SimPORTrace time tid tstep Nothing (EventReschedule control)) $
    assert (Down tid `PSQ.member` runqueue) $
    assert (tid `Map.member` threads) $
    invariant Nothing simstate $
    let thread = threads Map.! tid in
    assert (threadId thread == tid) $
    --assert (threadStep thread == tstep) $
    if threadStep thread /= tstep then
      error $ "Thread step out of sync\n"
           ++ "  runqueue:    "++show runqueue++"\n"
           ++ "  follows:     "++show tid++", step "++show tstep++"\n"
           ++ "  actual step: "++show (threadStep thread)++"\n"
           ++ "Thread:\n" ++ show thread ++ "\n"
    else
    schedule thread simstate { runqueue = PSQ.delete (Down tid) runqueue
                             , threads  = Map.delete tid threads }

-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule simstate@SimState{ runqueue, threads }
    | Just (Down !tid, _, _, runqueue') <- PSQ.minView runqueue =
    invariant Nothing simstate $

    let thread = threads Map.! tid in
    schedule thread simstate { runqueue = runqueue'
                             , threads  = Map.delete tid threads }

-- But when there are no runnable threads, we advance the time to the next
-- timer event, or stop.
reschedule simstate@SimState{ threads, timers, curTime = time, races } =
    invariant Nothing simstate $

    -- time is moving on
    --Debug.trace ("Rescheduling at "++show time++", "++
      --show (length (concatMap stepInfoRaces (activeRaces races++completeRaces races)))++" races") $

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (traceFinalRacesFound simstate $
                         TraceDeadlock time (labelledThreads threads))

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        written <- execAtomically' (runSTM $ mapM_ timeoutAction fired)
        (wakeupSTM, wokeby) <- threadsUnblockedByWrites written
        mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

        let wakeupThreadDelay = [ tid | TimerThreadDelay tid <- fired ]
            wakeup            = wakeupThreadDelay ++ wakeupSTM
            -- TODO: the vector clock below cannot be right, can it?
            (_, !simstate')   = unblockThreads bottomVClock wakeup simstate

            -- For each 'timeout' action where the timeout has fired, start a
            -- new thread to execute throwTo to interrupt the action.
            !timeoutExpired = [ (tid, tmid, isLockedRef)
                              | TimerTimeout tid tmid isLockedRef <- fired ]

        -- Get the isLockedRef values
        !timeoutExpired' <- traverse (\(tid, tmid, isLockedRef) -> do
                                        locked <- readSTRef isLockedRef
                                        return (tid, tmid, isLockedRef, locked)
                                    )
                                    timeoutExpired

        -- all open races will be completed and reported at this time
        !simstate'' <- forkTimeoutInterruptThreads timeoutExpired'
                                                   simstate' { races = noRaces }
        !trace <- reschedule simstate'' { curTime = time'
                                        , timers  = timers' }
        let traceEntries =
                     [ ( time', ThreadId [-1], -1, Just "timer"
                       , EventTimerFired tmid)
                     | (tmid, Timer _) <- zip tmids fired ]
                  ++ [ ( time', ThreadId [-1], -1, Just "register delay timer"
                       , EventRegisterDelayFired tmid)
                     | (tmid, TimerRegisterDelay _) <- zip tmids fired ]
                  ++ [ (time', tid', -1, tlbl', EventTxWakeup vids)
                     | tid' <- wakeupSTM
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
                  ++ [ ( time', tid, -1, Just "thread delay timer"
                       , EventThreadDelayFired)
                     | tid <- wakeupThreadDelay ]
                  ++ [ ( time', tid, -1, Just "timeout timer"
                       , EventTimeoutFired tmid)
                     | (tid, tmid, _, _) <- timeoutExpired' ]
                  ++ [ ( time', tid, -1, Just "forked thread"
                       , EventThreadForked tid)
                     | (tid, _, _, _) <- timeoutExpired' ]

        return $
          traceFinalRacesFound simstate $
          traceMany traceEntries trace
  where
    timeoutAction (Timer var) = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()
    timeoutAction (TimerRegisterDelay var) = writeTVar var True
    timeoutAction (TimerThreadDelay _)     = return ()
    timeoutAction (TimerTimeout _ _ _)     = return ()

unblockThreads :: forall s a.
                  VectorClock
               -> [ThreadId]
               -> SimState s a
               -> ([ThreadId], SimState s a)
unblockThreads vClock wakeup simstate@SimState {runqueue, threads} =
    -- To preserve our invariants (that threadBlocked is correct)
    -- we update the runqueue and threads together here
    ( unblockedIds
    , simstate { runqueue = foldr insertThread runqueue unblocked,
                 threads  = threads'
               })
  where
    -- can only unblock if the thread exists and is blocked (not running)
    unblocked :: [Thread s a]
    !unblocked = [ thread
                 | tid <- wakeup
                 , thread <-
                     case Map.lookup tid threads of
                       Just   Thread { threadDone    = True } -> [ ]
                       Just t@Thread { threadBlocked = True } -> [t]
                       _                                      -> [ ]
                 ]

    unblockedIds :: [ThreadId]
    !unblockedIds = map threadId unblocked

    -- and in which case we mark them as now running
    !threads'  = List.foldl'
                   (flip (Map.adjust
                     (\t -> t { threadBlocked = False,
                                threadVClock = vClock `leastUpperBoundVClock` threadVClock t })))
                   threads unblockedIds

-- | This function receives a list of TimerTimeout values that represent threads
-- for which the timeout expired and kills the running thread if needed.
--
-- This function is responsible for the second part of the race condition issue
-- and relates to the 'schedule's 'TimeoutFrame' locking explanation (here is
-- where the assassin threads are launched. So, as explained previously, at this
-- point in code, the timeout expired so we need to interrupt the running
-- thread. If the running thread finished at the same time the timeout expired
-- we have a race condition. To deal with this race condition what we do is
-- look at the lock value. If it is 'Locked' this means that the running thread
-- already finished (or won the race) so we can safely do nothing. Otherwise, if
-- the lock value is 'NotLocked' we need to acquire the lock and launch an
-- assassin thread that is going to interrupt the running one. Note that we
-- should run this interrupting thread in an unmasked state since it might
-- receive a 'ThreadKilled' exception.
--
forkTimeoutInterruptThreads :: [(ThreadId, TimeoutId, STRef s IsLocked, IsLocked)]
                            -> SimState s a
                            -> ST s (SimState s a)
forkTimeoutInterruptThreads timeoutExpired simState@SimState {threads} =
  foldlM (\st@SimState{ runqueue = runqueue,
                        threads  = threads'
                      }
           (t, isLockedRef)
          -> do
            let tid'      = threadId t
                threads'' = Map.insert tid' t threads'
                runqueue' = insertThread t runqueue
            writeSTRef isLockedRef (Locked tid')

            return st { runqueue = runqueue',
                        threads  = threads''
                      })
          simState
          throwToThread

  where
    -- can only throw exception if the thread exists and if the mutually
    -- exclusive lock exists and is still 'NotLocked'
    toThrow = [ (tid, tmid, ref, t)
              | (tid, tmid, ref, locked) <- timeoutExpired
              , Just t <- [Map.lookup tid threads]
              , NotLocked <- [locked]
              ]
    -- we launch a thread responsible for throwing an AsyncCancelled exception
    -- to the thread which timeout expired
    throwToThread =
      [ let nextId   = threadNextTId t
            tid'     = childThreadId tid nextId
         in ( Thread { threadId      = tid',
                       threadControl =
                        ThreadControl
                          (ThrowTo (toException (TimeoutException tmid))
                                    tid
                                    (Return ()))
                          ForkFrame,
                       threadBlocked = False,
                       threadDone    = False,
                       threadMasking = Unmasked,
                       threadThrowTo = [],
                       threadClockId = threadClockId t,
                       threadLabel   = Just "timeout-forked-thread",
                       threadNextTId = 1,
                       threadStep    = 0,
                       threadVClock  = insertVClock tid' 0
                                     $ threadVClock t,
                       threadEffect  = mempty,
                       threadRacy    = threadRacy t
                     }
            , ref)
      | (tid, tmid, ref, t) <- toThrow
      ]

-- | Iterate through the control stack to find an enclosing exception handler
-- of the right type, or unwind all the way to the top level for the thread.
--
-- Also return if it's the main thread or a forked thread since we handle the
-- cases differently.
--
unwindControlStack :: forall s a.
                      SomeException
                   -> Thread s a
                   -> Either Bool (Thread s a)
unwindControlStack e thread =
    case threadControl thread of
      ThreadControl _ ctl -> unwind (threadMasking thread) ctl
  where
    unwind :: forall s' c. MaskingState
           -> ControlStack s' c a
           -> Either Bool (Thread s' a)
    unwind _  MainFrame                 = Left True
    unwind _  ForkFrame                 = Left False
    unwind _ (MaskFrame _k maskst' ctl) = unwind maskst' ctl

    unwind maskst (CatchFrame handler k ctl) =
      case fromException e of
        -- not the right type, unwind to the next containing handler
        Nothing -> unwind maskst ctl

        -- Ok! We will be able to continue the thread with the handler
        -- followed by the continuation after the catch
        Just e' -> Right ( thread {
                      -- As per async exception rules, the handler is run
                      -- masked
                     threadControl = ThreadControl (handler e')
                                                   (MaskFrame k maskst ctl),
                     threadMasking = atLeastInterruptibleMask maskst
                   }
                )

    -- Either Timeout fired or the action threw an exception.
    -- - If Timeout fired, then it was possibly during this thread's execution
    --   so we need to run the continuation with a Nothing value.
    -- - If the timeout action threw an exception we need to keep unwinding the
    --   control stack looking for a handler to this exception.
    unwind maskst (TimeoutFrame tmid isLockedRef k ctl) =
      case fromException e of
        -- Exception came from timeout expiring
        Just (TimeoutException tmid') ->
          assert (tmid == tmid')
          Right thread { threadControl = ThreadControl (k Nothing) ctl }
        -- Exception came from a different exception
        _ -> unwind maskst ctl

    atLeastInterruptibleMask :: MaskingState -> MaskingState
    atLeastInterruptibleMask Unmasked = MaskedInterruptible
    atLeastInterruptibleMask ms       = ms


removeMinimums :: (Ord k, Ord p)
               => OrdPSQ k p a
               -> Maybe ([k], p, [a], OrdPSQ k p a)
removeMinimums = \psq ->
    case PSQ.minView psq of
      Nothing              -> Nothing
      Just (k, p, x, psq') -> Just (collectAll [k] p [x] psq')
  where
    collectAll ks p xs psq =
      case PSQ.minView psq of
        Just (k, p', x, psq')
          | p == p' -> collectAll (k:ks) p (x:xs) psq'
        _           -> (reverse ks, p, reverse xs, psq)

traceMany :: [(Time, ThreadId, Int, Maybe ThreadLabel, SimEventType)]
          -> SimTrace a -> SimTrace a
traceMany []                                   trace = trace
traceMany ((time, tid, tstep, tlbl, event):ts) trace =
    SimPORTrace time tid tstep tlbl event (traceMany ts trace)

lookupThreadLabel :: ThreadId -> Map ThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in 'ST' monad.  One can
-- recover failures or the result from 'SimTrace' with 'traceResult', or access
-- 'TraceEvent's generated by the computation with 'traceEvents'.  A slightly
-- more convenient way is exposed by 'runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (SimTrace a)
runSimTraceST mainAction = controlSimTraceST Nothing ControlDefault mainAction

controlSimTraceST :: Maybe Int -> ScheduleControl -> IOSim s a -> ST s (SimTrace a)
controlSimTraceST limit control mainAction =
  SimPORTrace (curTime initialState)
           (threadId mainThread)
           0
           (threadLabel mainThread)
           (EventSimStart control)
  <$> schedule mainThread initialState { control  = control,
                                         control0 = control,
                                         perStepTimeLimit = limit
                                       }
  where
    mainThread =
      Thread {
        threadId      = ThreadId [],
        threadControl = ThreadControl (runIOSim mainAction) MainFrame,
        threadBlocked = False,
        threadDone    = False,
        threadMasking = Unmasked,
        threadThrowTo = [],
        threadClockId = ClockId [],
        threadLabel   = Just "main",
        threadNextTId = 1,
        threadStep    = 0,
        threadVClock  = insertVClock (ThreadId []) 0 bottomVClock,
        threadEffect  = mempty,
        threadRacy    = False
      }


--
-- Executing STM Transactions
--

execAtomically :: forall s a c.
                  Time
               -> ThreadId
               -> Maybe ThreadLabel
               -> TVarId
               -> StmA s a
               -> (StmTxResult s a -> ST s (SimTrace c))
               -> ST s (SimTrace c)
execAtomically time tid tlbl nextVid0 action0 k0 =
    go AtomicallyFrame Map.empty Map.empty [] [] nextVid0 action0
  where
    go :: forall b.
          StmStack s b a
       -> Map TVarId (SomeTVar s)  -- set of vars read
       -> Map TVarId (SomeTVar s)  -- set of vars written
       -> [SomeTVar s]             -- vars written in order (no dups)
       -> [SomeTVar s]             -- vars created in order
       -> TVarId                   -- var fresh name supply
       -> StmA s b
       -> ST s (SimTrace c)
    go !ctl !read !written !writtenSeq !createdSeq !nextVid action = assert localInvariant $
                                                       case action of
      ReturnStm x ->
        {-# SCC "execAtomically.go.ReturnStm" #-}
        case ctl of
        AtomicallyFrame -> do
          -- Trace each created TVar
          !ds  <- traverse (\(SomeTVar tvar) -> traceTVarST tvar True) createdSeq
          -- Trace & commit each TVar
          !ds' <- Map.elems <$> traverse
                    (\(SomeTVar tvar) -> do
                        tr <- traceTVarST tvar False
                        !_ <- commitTVar tvar
                        -- Also assert the data invariant that outside a tx
                        -- the undo stack is empty:
                        undos <- readTVarUndos tvar
                        assert (null undos) $ return tr
                    ) written

          -- Return the vars written, so readers can be unblocked
          k0 $ StmTxCommitted x (reverse writtenSeq)
                                (Map.elems read)
                                (reverse createdSeq)
                                (mapMaybe (\TraceValue { traceDynamic }
                                            -> toDyn <$> traceDynamic)
                                          $ ds ++ ds')
                                (mapMaybe traceString $ ds ++ ds')
                                nextVid

        BranchFrame _b k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
          -- The branch has successfully completed the transaction. Hence,
          -- the alternative branch can be ignored.
          -- Commit the TVars written in this sub-transaction that are also
          -- in the written set of the outer transaction
          !_ <- traverse_ (\(SomeTVar tvar) -> commitTVar tvar)
                          (Map.intersection written writtenOuter)
          -- Merge the written set of the inner with the outer
          let written'    = Map.union written writtenOuter
              writtenSeq' = filter (\(SomeTVar tvar) ->
                                      tvarId tvar `Map.notMember` writtenOuter)
                                    writtenSeq
                         ++ writtenOuterSeq
              createdSeq' = createdSeq ++ createdOuterSeq
          -- Skip the orElse right hand and continue with the k continuation
          go ctl' read written' writtenSeq' createdSeq' nextVid (k x)

      ThrowStm e ->
        {-# SCC "execAtomically.go.ThrowStm" #-} do
        -- Revert all the TVar writes
        !_ <- traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
        case ctl of
          AtomicallyFrame -> do
            k0 $ StmTxAborted (Map.elems read) (toException e)

          BranchFrame (CatchStmA h) k writtenOuter writtenOuterSeq createdOuterSeq ctl' ->
            {-# SCC "execAtomically.go.BranchFrame" #-} do
            -- Execute the left side in a new frame with an empty written set.
            -- but preserve ones that were set prior to it, as specified in the
            -- [stm](https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#v:catchSTM) package.
            let ctl'' = BranchFrame NoOpStmA k writtenOuter writtenOuterSeq createdOuterSeq ctl'
            go ctl'' read Map.empty [] [] nextVid (h e)

          BranchFrame (OrElseStmA _r) _k writtenOuter writtenOuterSeq createdOuterSeq ctl' ->
            {-# SCC "execAtomically.go.BranchFrame" #-} do
            go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid (ThrowStm e)

          BranchFrame NoOpStmA _k writtenOuter writtenOuterSeq createdOuterSeq ctl' ->
            {-# SCC "execAtomically.go.BranchFrame" #-} do
            go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid (ThrowStm e)

      CatchStm a h k ->
        {-# SCC "execAtomically.go.ThrowStm" #-} do
        -- Execute the left side in a new frame with an empty written set
        let ctl' = BranchFrame (CatchStmA h) k written writtenSeq createdSeq ctl
        go ctl' read Map.empty [] [] nextVid a

      Retry ->
        {-# SCC "execAtomically.go.Retry" #-} do
        -- Always revert all the TVar writes for the retry
        !_ <- traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
        case ctl of
          AtomicallyFrame -> do
            -- Return vars read, so the thread can block on them
            k0 $! StmTxBlocked $! Map.elems read

          BranchFrame (OrElseStmA b) k writtenOuter writtenOuterSeq createdOuterSeq ctl' ->
            {-# SCC "execAtomically.go.BranchFrame.OrElseStmA" #-} do
            -- Execute the orElse right hand with an empty written set
            let ctl'' = BranchFrame NoOpStmA k writtenOuter writtenOuterSeq createdOuterSeq ctl'
            go ctl'' read Map.empty [] [] nextVid b

          BranchFrame _ _k writtenOuter writtenOuterSeq createdOuterSeq ctl' ->
            {-# SCC "execAtomically.go.BranchFrame" #-} do
            -- Retry makes sense only within a OrElse context. If it is a branch other than
            -- OrElse left side, then bubble up the `retry` to the frame above.
            -- Skip the continuation and propagate the retry into the outer frame
            -- using the written set for the outer frame
            go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid Retry

      OrElse a b k ->
        {-# SCC "execAtomically.go.OrElse" #-} do
        -- Execute the left side in a new frame with an empty written set
        let ctl' = BranchFrame (OrElseStmA b) k written writtenSeq createdSeq ctl
        go ctl' read Map.empty [] [] nextVid a

      NewTVar !mbLabel x k ->
        {-# SCC "execAtomically.go.NewTVar" #-} do
        !v <- execNewTVar nextVid mbLabel x
        -- record a write to the TVar so we know to update its VClock
        let written' = Map.insert (tvarId v) (SomeTVar v) written
        -- save the value: it will be committed or reverted
        !_ <- saveTVar v
        go ctl read written' writtenSeq (SomeTVar v : createdSeq) (succ nextVid) (k v)

      LabelTVar !label tvar k ->
        {-# SCC "execAtomically.go.LabelTVar" #-} do
        !_ <- writeSTRef (tvarLabel tvar) $! (Just label)
        go ctl read written writtenSeq createdSeq nextVid k

      TraceTVar tvar f k ->
        {-# SCC "execAtomically.go.TraceTVar" #-} do
        !_ <- writeSTRef (tvarTrace tvar) (Just f)
        go ctl read written writtenSeq createdSeq nextVid k

      ReadTVar v k
        | tvarId v `Map.member` read ->
            {-# SCC "execAtomically.go.ReadTVar" #-} do
            x <- execReadTVar v
            go ctl read written writtenSeq createdSeq nextVid (k x)
        | otherwise ->
            {-# SCC "execAtomically.go.ReadTVar" #-} do
            x <- execReadTVar v
            let read' = Map.insert (tvarId v) (SomeTVar v) read
            go ctl read' written writtenSeq createdSeq nextVid (k x)

      WriteTVar v x k
        | tvarId v `Map.member` written ->
            {-# SCC "execAtomically.go.WriteTVar" #-} do
            !_ <- execWriteTVar v x
            go ctl read written writtenSeq createdSeq nextVid k
        | otherwise ->
            {-# SCC "execAtomically.go.WriteTVar" #-} do
            !_ <- saveTVar v
            !_ <- execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go ctl read written' (SomeTVar v : writtenSeq) createdSeq nextVid k

      SayStm msg k ->
        {-# SCC "execAtomically.go.SayStm" #-} do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        -- TODO: step
        return $ SimPORTrace time tid (-1) tlbl (EventSay msg) trace

      OutputStm x k ->
        {-# SCC "execAtomically.go.OutputStm" #-} do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        -- TODO: step
        return $ SimPORTrace time tid (-1) tlbl (EventLog x) trace

      LiftSTStm st k ->
        {-# SCC "schedule.LiftSTStm" #-} do
        x <- strictToLazyST st
        go ctl read written writtenSeq createdSeq nextVid (k x)

      FixStm f k ->
        {-# SCC "execAtomically.go.FixStm" #-} do
        r <- newSTRef (throw NonTermination)
        x <- unsafeInterleaveST $ readSTRef r
        let k' = unSTM (f x) $ \x' ->
                    LiftSTStm (lazyToStrictST (writeSTRef r x')) (\() -> k x')
        go ctl read written writtenSeq createdSeq nextVid k'

      where
        localInvariant =
            Map.keysSet written
         == Set.fromList ([ tvarId tvar | SomeTVar tvar <- writtenSeq ]
                       ++ [ tvarId tvar | SomeTVar tvar <- createdSeq ])


-- | Special case of 'execAtomically' supporting only var reads and writes
--
execAtomically' :: StmA s () -> ST s [SomeTVar s]
execAtomically' = go Map.empty
  where
    go :: Map TVarId (SomeTVar s)  -- set of vars written
       -> StmA s ()
       -> ST s [SomeTVar s]
    go !written action = case action of
      ReturnStm () -> do
        !_ <- traverse_ (\(SomeTVar tvar) -> commitTVar tvar) written
        return (Map.elems written)
      ReadTVar v k  -> do
        x <- execReadTVar v
        go written (k x)
      WriteTVar v x k
        | tvarId v `Map.member` written -> do
            !_ <- execWriteTVar v x
            go written k
        | otherwise -> do
            !_ <- saveTVar v
            !_ <- execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go written' k
      _ -> error "execAtomically': only for special case of reads and writes"


execNewTVar :: TVarId -> Maybe String -> a -> ST s (TVar s a)
execNewTVar nextVid !mbLabel x = do
    tvarLabel   <- newSTRef mbLabel
    tvarCurrent <- newSTRef x
    tvarUndo    <- newSTRef []
    tvarBlocked <- newSTRef ([], Set.empty)
    tvarVClock  <- newSTRef bottomVClock
    tvarTrace   <- newSTRef Nothing
    return TVar {tvarId = nextVid, tvarLabel,
                 tvarCurrent, tvarUndo, tvarBlocked, tvarVClock,
                 tvarTrace}

execReadTVar :: TVar s a -> ST s a
execReadTVar TVar{tvarCurrent} = readSTRef tvarCurrent
{-# INLINE execReadTVar #-}

execWriteTVar :: TVar s a -> a -> ST s ()
execWriteTVar TVar{tvarCurrent} = writeSTRef tvarCurrent
{-# INLINE execWriteTVar #-}

saveTVar :: TVar s a -> ST s ()
saveTVar TVar{tvarCurrent, tvarUndo} = do
    -- push the current value onto the undo stack
    v  <- readSTRef tvarCurrent
    vs <- readSTRef tvarUndo
    writeSTRef tvarUndo (v:vs)

revertTVar :: TVar s a -> ST s ()
revertTVar TVar{tvarCurrent, tvarUndo} = do
    -- pop the undo stack, and revert the current value
    vs <- readSTRef tvarUndo
    writeSTRef tvarCurrent (head vs)
    writeSTRef tvarUndo    (tail vs)
{-# INLINE revertTVar #-}

commitTVar :: TVar s a -> ST s ()
commitTVar TVar{tvarUndo} = do
    vs <- readSTRef tvarUndo
    -- pop the undo stack, leaving the current value unchanged
    writeSTRef tvarUndo (tail vs)
{-# INLINE commitTVar #-}

readTVarUndos :: TVar s a -> ST s [a]
readTVarUndos TVar{tvarUndo} = readSTRef tvarUndo

-- | Trace a 'TVar'.  It must be called only on 'TVar's that were new or
-- 'written.
traceTVarST :: TVar s a
            -> Bool -- true if it's a new 'TVar'
            -> ST s TraceValue
traceTVarST TVar{tvarCurrent, tvarUndo, tvarTrace} new = do
    mf <- readSTRef tvarTrace
    case mf of
      Nothing -> return TraceValue { traceDynamic = (Nothing :: Maybe ()), traceString = Nothing }
      Just f  -> do
        vs <- readSTRef tvarUndo
        v <-  readSTRef tvarCurrent
        case (new, vs) of
          (True, _) -> f Nothing v
          (_, _:_)  -> f (Just $ last vs) v
          _         -> error "traceTVarST: unexpected tvar state"



leastUpperBoundTVarVClocks :: [SomeTVar s] -> ST s VectorClock
leastUpperBoundTVarVClocks tvars =
  foldr leastUpperBoundVClock bottomVClock <$>
    sequence [readSTRef (tvarVClock r) | SomeTVar r <- tvars]

--
-- Blocking and unblocking on TVars
--

readTVarBlockedThreads :: TVar s a -> ST s [ThreadId]
readTVarBlockedThreads TVar{tvarBlocked} = fst <$> readSTRef tvarBlocked

blockThreadOnTVar :: ThreadId -> TVar s a -> ST s ()
blockThreadOnTVar tid TVar{tvarBlocked} = do
    (tids, tidsSet) <- readSTRef tvarBlocked
    when (tid `Set.notMember` tidsSet) $ do
      let !tids'    = tid : tids
          !tidsSet' = Set.insert tid tidsSet
      writeSTRef tvarBlocked (tids', tidsSet')

unblockAllThreadsFromTVar :: TVar s a -> ST s ()
unblockAllThreadsFromTVar TVar{tvarBlocked} = do
    writeSTRef tvarBlocked ([], Set.empty)

-- | For each TVar written to in a transaction (in order) collect the threads
-- that blocked on each one (in order).
--
-- Also, for logging purposes, return an association between the threads and
-- the var writes that woke them.
--
threadsUnblockedByWrites :: [SomeTVar s]
                         -> ST s ([ThreadId], Map ThreadId (Set (Labelled TVarId)))
threadsUnblockedByWrites written = do
  tidss <- sequence
             [ (,) <$> labelledTVarId tvar <*> readTVarBlockedThreads tvar
             | SomeTVar tvar <- written ]
  -- Threads to wake up, in wake up order, annotated with the vars written that
  -- caused the unblocking.
  -- We reverse the individual lists because the tvarBlocked is used as a stack
  -- so it is in order of last written, LIFO, and we want FIFO behaviour.
  let wakeup = ordNub [ tid | (_vid, tids) <- tidss, tid <- reverse tids ]
      wokeby = Map.fromListWith Set.union
                                [ (tid, Set.singleton vid)
                                | (vid, tids) <- tidss
                                , tid <- tids ]
  return (wakeup, wokeby)

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go !_ [] = []
    go !s (x:xs)
      | x `Set.member` s = go s xs
      | otherwise        = x : go (Set.insert x s) xs

--
-- Steps
--

data Step = Step {
    stepThreadId :: !ThreadId,
    stepStep     :: !Int,
    stepEffect   :: !Effect,
    stepVClock   :: !VectorClock
  }
  deriving Show

-- steps race if they can be reordered with a possibly different outcome
racingSteps :: Step -- ^ an earlier step
            -> Step -- ^ a later step
            -> Bool
racingSteps s s' =
     stepThreadId s /= stepThreadId s'
  && not (stepThreadId s' `elem` effectWakeup (stepEffect s))
  && (stepEffect s `racingEffects` stepEffect s'
   || throwsTo s s'
   || throwsTo s' s)
  where throwsTo s1 s2 =
             stepThreadId s2 `elem` effectThrows (stepEffect s1)
          && stepEffect s2 /= mempty

currentStep :: Thread s a -> Step
currentStep Thread { threadId     = tid,
                     threadStep   = tstep,
                     threadEffect = teffect,
                     threadVClock = vClock
                   } =
  Step { stepThreadId = tid,
         stepStep     = tstep,
         stepEffect   = teffect,
         stepVClock   = vClock
       }

stepThread :: Thread s a -> Thread s a
stepThread thread@Thread { threadId     = tid,
                           threadStep   = tstep,
                           threadVClock = vClock } =
  thread { threadStep   = tstep+1,
           threadEffect = mempty,
           threadVClock = insertVClock tid (tstep+1) vClock
         }

-- As we run a simulation, we collect info about each previous step
data StepInfo = StepInfo {
    stepInfoStep       :: Step,
    -- Control information when we reached this step
    stepInfoControl    :: ScheduleControl,
    -- threads that are still concurrent with this step
    stepInfoConcurrent :: Set ThreadId,
    -- steps following this one that did not happen after it
    -- (in reverse order)
    stepInfoNonDep     :: [Step],
    -- later steps that race with this one
    stepInfoRaces      :: [Step]
  }
  deriving Show

--
-- Races
--

data Races = Races { -- These steps may still race with future steps
                     activeRaces   :: ![StepInfo],
                     -- These steps cannot be concurrent with future steps
                     completeRaces :: ![StepInfo]
                   }
  deriving Show

noRaces :: Races
noRaces = Races [] []

updateRacesInSimState :: Thread s a -> SimState s a -> Races
updateRacesInSimState thread SimState{ control, threads, races } =
    traceRaces $
    updateRaces step
                (threadBlocked thread)
                control
                (Map.keysSet (Map.filter (\t -> not (threadDone t)
                                             && threadId t `Set.notMember`
                                                effectForks (stepEffect step)
                                         ) threads))
                races
  where
    step = currentStep thread

-- | 'updateRaces' turns a current 'Step' into 'StepInfo', and updates all
-- 'activeRaces'.
--
-- We take care that steps can only race against threads in their
-- concurrent set. When this becomes empty, a step can be retired into
-- the "complete" category, but only if there are some steps racing
-- with it.
updateRaces :: Step -> Bool -> ScheduleControl -> Set ThreadId -> Races -> Races
updateRaces newStep@Step{ stepThreadId = tid, stepEffect = newEffect }
            blocking
            control
            newConcurrent0
            races@Races{ activeRaces } =

  let justBlocking :: Bool
      justBlocking = blocking && onlyReadEffect newEffect

      -- a new step cannot race with any threads that it just woke up
      new :: [StepInfo]
      !new | isNotRacyThreadId tid  = []  -- non-racy threads do not race
           | Set.null newConcurrent = []  -- cannot race with anything
           | justBlocking           = []  -- no need to defer a blocking transaction
           | otherwise              =
               [StepInfo { stepInfoStep       = newStep,
                           stepInfoControl    = control,
                           stepInfoConcurrent = newConcurrent,
                           stepInfoNonDep     = [],
                           stepInfoRaces      = []
                         }]
        where
          newConcurrent :: Set ThreadId
          newConcurrent = foldr Set.delete newConcurrent0 (effectWakeup newEffect)

      activeRaces' :: [StepInfo]
      !activeRaces' =
        [ -- if this step depends on the previous step, or is not concurrent,
          -- then any threads that it wakes up become non-concurrent also.
          let !lessConcurrent = foldr Set.delete concurrent (effectWakeup newEffect) in
          if tid `elem` concurrent then
            let theseStepsRace = isRacyThreadId tid && racingSteps step newStep
                happensBefore  = step `happensBeforeStep` newStep
                !nondep' | happensBefore = nondep
                         | otherwise     = newStep : nondep
                -- We will only record the first race with each thread---reversing
                -- the first race makes the next race detectable. Thus we remove a
                -- thread from the concurrent set after the first race.
                concurrent' | happensBefore  = Set.delete tid lessConcurrent
                            | theseStepsRace = Set.delete tid concurrent
                            | otherwise      = concurrent
                -- Here we record discovered races.
                -- We only record a new race if we are following the default schedule,
                -- to avoid finding the same race in different parts of the search space.
                !stepRaces' | (control == ControlDefault ||
                               control == ControlFollow [] []) &&
                              theseStepsRace  = newStep : stepRaces
                            | otherwise       = stepRaces

            in stepInfo { stepInfoConcurrent = effectForks newEffect
                                             `Set.union` concurrent',
                          stepInfoNonDep     = nondep',
                          stepInfoRaces      = stepRaces'
                        }

          else stepInfo { stepInfoConcurrent = lessConcurrent }

        | !stepInfo@StepInfo { stepInfoStep       = step,
                               stepInfoConcurrent = concurrent,
                               stepInfoNonDep     = nondep,
                               stepInfoRaces      = stepRaces
                            }
            <- activeRaces ]
  in normalizeRaces $ races { activeRaces = new ++ activeRaces' }

-- When a thread terminates, we remove it from the concurrent thread
-- sets of active races.

threadTerminatesRaces :: ThreadId -> Races -> Races
threadTerminatesRaces tid races@Races{ activeRaces } =
  let activeRaces' = [ s{stepInfoConcurrent = Set.delete tid stepInfoConcurrent}
                     | s@StepInfo{ stepInfoConcurrent } <- activeRaces ]
  in normalizeRaces $ races{ activeRaces = activeRaces' }

normalizeRaces :: Races -> Races
normalizeRaces Races{ activeRaces, completeRaces } =
  let !activeRaces'   = filter (not . null. stepInfoConcurrent) activeRaces
      !completeRaces' = filter (not . null. stepInfoRaces)
                          (filter (null . stepInfoConcurrent) activeRaces)
                     ++ completeRaces
  in Races{ activeRaces = activeRaces', completeRaces = completeRaces' }

-- We assume that steps do not race with later steps after a quiescent
-- period. Quiescent periods end when simulated time advances, thus we
-- are assuming here that all work is completed before a timer
-- triggers.

quiescentRaces :: Races -> Races
quiescentRaces Races{ activeRaces, completeRaces } =
  Races{ activeRaces = [],
         completeRaces = [ s{stepInfoConcurrent = Set.empty}
                         | s <- activeRaces
                         , not (null (stepInfoRaces s))
                         ] ++ completeRaces }

traceRaces :: Races -> Races
traceRaces r = r
-- traceRaces r@Races{activeRaces,completeRaces} =
--   Debug.trace ("Tracking "++show (length (concatMap stepInfoRaces activeRaces)) ++" races") r


--
-- Schedule control
--

controlTargets :: StepId -> ScheduleControl -> Bool
controlTargets stepId
               (ControlAwait (ScheduleMod{ scheduleModTarget }:_)) =
  stepId == scheduleModTarget
controlTargets _stepId _ = False

followControl :: ScheduleControl -> ScheduleControl
followControl (ControlAwait (ScheduleMod { scheduleModInsertion } : mods)) =
               ControlFollow scheduleModInsertion mods
followControl (ControlAwait []) = error "Impossible: followControl (ControlAwait [])"
followControl ControlDefault{}  = error "Impossible: followControl ControlDefault{}"
followControl ControlFollow{}   = error "Impossible: followControl ControlFollow{}"

controlFollows :: StepId -> ScheduleControl -> Bool
controlFollows _stepId  ControlDefault               = True
controlFollows _stepId (ControlFollow [] _)          = True
controlFollows stepId  (ControlFollow (stepId':_) _) = stepId == stepId'
controlFollows stepId  (ControlAwait (smod:_))       = stepId /= scheduleModTarget smod
controlFollows _       (ControlAwait [])             = error "Impossible: controlFollows _ (ControlAwait [])"

advanceControl :: StepId -> ScheduleControl -> ScheduleControl
advanceControl (tid,step) control@(ControlFollow ((tid',step'):sids') tgts)
  | tid /= tid' =
      -- we are switching threads to follow the schedule
      --Debug.trace ("Switching threads from "++show (tid,step)++" to "++show (tid',step')++"\n") $
      control
  | step == step' =
      ControlFollow sids' tgts
  | otherwise =
      error $ concat
            [ "advanceControl ", show (tid,step)
            , " cannot follow step ", show step'
            , "\n"
            ]
advanceControl stepId (ControlFollow [] []) =
  ControlDefault
advanceControl stepId (ControlFollow [] tgts) =
  ControlAwait tgts
advanceControl stepId control =
  assert (not $ controlTargets stepId control) $
  control

--
-- Schedule modifications
--

stepStepId :: Step -> (ThreadId, Int)
stepStepId Step{ stepThreadId = tid, stepStep = n } = (tid,n)

stepInfoToScheduleMods :: StepInfo -> [ScheduleMod]
stepInfoToScheduleMods
  StepInfo{ stepInfoStep    = step,
            stepInfoControl = control,
            stepInfoNonDep  = nondep,
            stepInfoRaces   = races
          } =
  -- It is actually possible for a later step that races with an earlier one
  -- not to *depend* on it in a happens-before sense. But we don't want to try
  -- to follow any steps *after* the later one.
  [ ScheduleMod
      { scheduleModTarget    = stepStepId step
      , scheduleModControl   = control
      , scheduleModInsertion = takeWhile (/=stepStepId step')
                                         (map stepStepId (reverse nondep))
                            ++ [stepStepId step']
                            -- It should be unnecessary to include the delayed
                            -- step in the insertion, since the default
                            -- scheduling should run it anyway. Removing it may
                            -- help avoid redundant schedules.
                            -- ++ [stepStepId step]
      }
  | step' <- races ]

traceFinalRacesFound :: SimState s a -> SimTrace a -> SimTrace a
traceFinalRacesFound SimState{ control0 = control, races } =
    TraceRacesFound [extendScheduleControl control m | m <- scheduleMods]
  where
    scheduleMods :: [ScheduleMod]
    scheduleMods =
        concatMap stepInfoToScheduleMods
      . completeRaces
      . quiescentRaces
      $ races

-- Extend an existing schedule control with a newly discovered schedule mod
extendScheduleControl' :: ScheduleControl -> ScheduleMod -> ScheduleControl
extendScheduleControl' ControlDefault m = ControlAwait [m]
extendScheduleControl' (ControlAwait mods) m =
  case scheduleModControl m of
    ControlDefault     -> ControlAwait (mods++[m])
    ControlAwait mods' ->
      let common = length mods - length mods' in
      assert (common >= 0 && drop common mods==mods') $
      ControlAwait (take common mods++[m{ scheduleModControl = ControlDefault }])
    ControlFollow stepIds mods' ->
      let common = length mods - length mods' - 1
          m'     = mods !! common
          isUndo = scheduleModTarget m' `elem` scheduleModInsertion m
          m''    = m'{ scheduleModInsertion =
                         takeWhile (/=scheduleModTarget m)
                                   (scheduleModInsertion m')
                         ++
                         scheduleModInsertion m }
      in
      assert (common >= 0) $
      assert (drop (common+1) mods == mods') $
      if isUndo
        then ControlAwait mods          -- reject this mod... it's undoing a previous one
        else ControlAwait (take common mods++[m''])
extendScheduleControl' ControlFollow{} ScheduleMod{} =
  -- note: this case is impossible, since `extendScheduleControl'` first
  -- argument is either the initial `ControlDefault` or a result of calling
  -- `extendScheduleControl'` itself.
  error "Impossible: extendScheduleControl' ControlFollow{} ScheduleMod{}"

extendScheduleControl :: ScheduleControl -> ScheduleMod -> ScheduleControl
extendScheduleControl control m =
  let control' = extendScheduleControl' control m in
  {- Debug.trace (unlines ["",
                        "Extending "++show control,
                        "     with "++show m,
                        "   yields "++show control']) -}
              control'
