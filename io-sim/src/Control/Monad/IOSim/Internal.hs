{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Control.Monad.IOSim.Internal
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
  , Trace.Trace (SimTrace, Trace, TraceMainReturn, TraceMainException, TraceDeadlock)
  , SimEvent (..)
  , SimResult (..)
  , SimEventType (..)
  , TraceEvent
  , ppTrace
  , ppTrace_
  , ppSimEvent
  , liftST
  , execReadTVar
  ) where

import           Prelude hiding (read)

import           Data.Dynamic
import           Data.Foldable (foldlM, toList, traverse_)
import qualified Data.List as List
import qualified Data.List.Trace as Trace
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Deque.Strict (Deque)
import qualified Deque.Strict as Deque

import           GHC.Exts (fromList)
import qualified GHC.Conc as GHC (ThreadStatus(..))

import           Control.Exception (AsyncException (..), NonTermination (..),
                     assert, throw)
import           Control.Monad (join, when)
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Lazy.Unsafe (unsafeIOToST, unsafeInterleaveST)
import           Data.STRef.Lazy

import           Control.Concurrent.Class.MonadSTM.TVar hiding (TVar)
import           Control.Monad.Class.MonadSTM hiding (STM)
import           Control.Monad.Class.MonadThrow hiding (getMaskingState)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Monad.IOSim.InternalTypes
import           Control.Monad.IOSim.Types hiding (SimEvent (SimPOREvent),
                     Trace (SimPORTrace))
import           Control.Monad.IOSim.Types (SimEvent)

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !ThreadId,
    threadControl :: !(ThreadControl s a),
    threadStatus  :: !ThreadStatus,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, Labelled ThreadId)],
    threadClockId :: !ClockId,
    threadLabel   ::  Maybe ThreadLabel,
    threadNextTId :: !Int
  }

isThreadBlocked :: Thread s a -> Bool
isThreadBlocked t = case threadStatus t of
    ThreadBlocked {} -> True
    _                -> False

labelledTVarId :: TVar s a -> ST s (Labelled TVarId)
labelledTVarId TVar { tvarId, tvarLabel } = (Labelled tvarId) <$> readSTRef tvarLabel

labelledThreads :: Map ThreadId (Thread s a) -> [Labelled ThreadId]
labelledThreads threadMap =
    -- @Map.foldr'@ (and alikes) are not strict enough, to not ratain the
    -- original thread map we need to evaluate the spine of the list.
    -- TODO: https://github.com/haskell/containers/issues/749
    Map.foldr'
      (\Thread { threadId, threadLabel } !acc -> Labelled threadId threadLabel : acc)
      [] threadMap


-- | Timers mutable variables. Supports 'newTimeout' api, the second
-- one 'registerDelay', the third one 'threadDelay'.
--
data TimerCompletionInfo s =
       Timer !(TVar s TimeoutState)
     -- ^ `newTimeout` timer.
     | TimerRegisterDelay !(TVar s Bool)
     -- ^ `registerDelay` timer.
     | TimerThreadDelay !ThreadId !TimeoutId
     -- ^ `threadDelay` timer run by `ThreadId` which was assigned the given
     -- `TimeoutId` (only used to report in a trace).
     | TimerTimeout !ThreadId !TimeoutId !(STRef s IsLocked)
     -- ^ `timeout` timer run by `ThreadId` which was assigned the given
     -- `TimeoutId` (only used to report in a trace).


type Timeouts s = OrdPSQ TimeoutId Time (TimerCompletionInfo s)

-- | Internal state.
--
data SimState s a = SimState {
       runqueue :: !(Deque ThreadId),
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads  :: !(Map ThreadId (Thread s a)),
       -- | Keep track of the reason threads finished for 'threadStatus'
       finished :: !(Map ThreadId FinishedReason),
       -- | current time
       curTime  :: !Time,
       -- | ordered list of timers and timeouts
       timers   :: !(Timeouts s),
       -- | list of clocks
       clocks   :: !(Map ClockId UTCTime),
       nextVid  :: !TVarId,     -- ^ next unused 'TVarId'
       nextTmid :: !TimeoutId   -- ^ next unused 'TimeoutId'
     }

initialState :: SimState s a
initialState =
    SimState {
      runqueue = mempty,
      threads  = Map.empty,
      finished = Map.empty,
      curTime  = Time 0,
      timers   = PSQ.empty,
      clocks   = Map.singleton (ClockId []) epoch1970,
      nextVid  = TVarId 0,
      nextTmid = TimeoutId 0
    }
  where
    epoch1970 = UTCTime (fromGregorian 1970 1 1) 0

invariant :: Maybe (Thread s a) -> SimState s a -> x -> x

invariant (Just running) simstate@SimState{runqueue,threads,clocks} =
   assert (not (isThreadBlocked running))
 . assert (threadId running `Map.notMember` threads)
 . assert (threadId running `List.notElem` runqueue)
 . assert (threadClockId running `Map.member` clocks)
 . invariant Nothing simstate

invariant Nothing SimState{runqueue,threads,clocks} =
   assert (all (`Map.member` threads) runqueue)
 . assert (and [ isThreadBlocked t == (threadId t `notElem` runqueue)
               | t <- Map.elems threads ])
 . assert (toList runqueue == List.nub (toList runqueue))
 . assert (and [ threadClockId t `Map.member` clocks
               | t <- Map.elems threads ])

-- | Interpret the simulation monotonic time as a 'NominalDiffTime' since
-- the start.
timeSinceEpoch :: Time -> NominalDiffTime
timeSinceEpoch (Time t) = fromRational (toRational t)


-- | Schedule / run a thread.
--
schedule :: Thread s a -> SimState s a -> ST s (SimTrace a)
schedule !thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl,
           threadMasking = maskst,
           threadLabel   = tlbl
         }
         !simstate@SimState {
           runqueue,
           threads,
           finished,
           timers,
           clocks,
           nextVid, nextTmid,
           curTime  = time
         } =
  invariant (Just thread) simstate $
  case action of

    Return x -> {-# SCC "schedule.Return" #-}
                case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ SimTrace time tid tlbl EventThreadFinished
               $ TraceMainReturn time x (labelledThreads threads)

      ForkFrame -> do
        -- this thread is done
        !trace <- deschedule (Terminated FinishedNormally) thread simstate
        return $ SimTrace time tid tlbl EventThreadFinished
               $ SimTrace time tid tlbl (EventDeschedule $ Terminated FinishedNormally)
               $ trace

      MaskFrame k maskst' ctl' -> do
        -- pop the control stack, restore thread-local state
        let thread' = thread { threadControl = ThreadControl (k x) ctl'
                             , threadMasking = maskst' }
        -- but if we're now unmasked, check for any pending async exceptions
        !trace <- deschedule Interruptable thread' simstate
        return $ SimTrace time tid tlbl (EventMask maskst')
               $ SimTrace time tid tlbl (EventDeschedule Interruptable)
               $ trace

      CatchFrame _handler k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

      TimeoutFrame tmid isLockedRef k ctl' -> do
        -- There is a possible race between timeout action and the timeout expiration.
        -- We use a lock to solve the race.
        --
        -- The lock starts 'NotLocked' and when the timeout fires the lock is
        -- locked and asynchronously an assassin thread is coming to interrupt
        -- it. If the lock is locked when the timeout is fired then nothing
        -- happens.
        --
        -- Knowing this, if we reached this point in the code and the lock is
        -- 'Locked', then it means that this thread still hasn't received the
        -- 'TimeoutException', so we need to kill the thread that is responsible
        -- for doing that (the assassin thread, we need to defend ourselves!)
        -- and run our continuation successfully and peacefully. We will do that
        -- by uninterruptibly-masking ourselves so we can not receive any
        -- exception and kill the assassin thread behind its back.
        -- If the lock is 'NotLocked' then it means we can just acquire it and
        -- carry on with the success case.
        locked <- readSTRef isLockedRef
        case locked of
          Locked etid -> do
            let -- Kill the assassin throwing thread then unmask exceptions and
                -- carry on the continuation
                thread' =
                  thread { threadControl =
                            ThreadControl (ThrowTo (toException ThreadKilled)
                                                   etid
                                                   (Return ()))
                                          (MaskFrame (\_ -> k (Just x)) maskst ctl')
                         , threadMasking = MaskedUninterruptible
                         }
            schedule thread' simstate

          NotLocked -> do
            -- Acquire lock
            writeSTRef isLockedRef (Locked tid)
            let -- Remove the timer from the queue
                timers' = PSQ.delete tmid timers
                -- Run the continuation
                thread' = thread { threadControl = ThreadControl (k (Just x)) ctl' }
            schedule thread' simstate { timers = timers' }

    Throw thrower e -> {-# SCC "schedule.Throw" #-}
               case unwindControlStack e thread timers' of
      -- Found a CatchFrame
      (Right thread'@Thread { threadMasking = maskst' }, timers'') -> do
        -- We found a suitable exception handler, continue with that
        trace <- schedule thread' simstate { timers = timers'' }
        return (SimTrace time tid tlbl (EventThrow e) $
                SimTrace time tid tlbl (EventMask maskst') trace)

      (Left isMain, timers'')
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain ->
          -- An unhandled exception in the main thread terminates the program
          return (SimTrace time tid tlbl (EventThrow e) $
                  SimTrace time tid tlbl (EventThreadUnhandled e) $
                  TraceMainException time e (labelledThreads threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          let reason | ThrowSelf <- thrower = FinishedNormally
                     | otherwise            = FinishedDied
          !trace <- deschedule (Terminated reason) thread simstate { timers = timers'' }
          return $ SimTrace time tid tlbl (EventThrow e)
                 $ SimTrace time tid tlbl (EventThreadUnhandled e)
                 $ SimTrace time tid tlbl (EventDeschedule $ Terminated reason)
                 $ trace
      where
        -- When we throw an exception we need to cancel thread delay action
        -- which could be blocking the current thread.  This is important for
        -- `TimeoutException` or any other asynchronous exceptions.
        --
        -- Note: we treat `TimerThreadDelay` different than `TimerTimeout`s.
        -- The first one cannot be stacked (there's not corresponding frame), so
        -- it's fine to just remove scheduled thread delay (there can be at most
        -- one).  `TimerTimeout` however is different and we need to remove them
        -- as we unwind the stack frames in `unwindControlStack`.
        timers' = PSQ.fromList
                . filter (\(_,_,info) -> case info of
                            TimerThreadDelay tid' _
                              -> tid' /= tid
                            _ -> True)
                . PSQ.toAscList
                $ timers

    Catch action' handler k ->
      {-# SCC "schedule.Catch" #-} do
      -- push the failure and success continuations onto the control stack
      let thread' = thread { threadControl = ThreadControl action'
                                               (CatchFrame handler k ctl) }
      schedule thread' simstate

    Evaluate expr k ->
      {-# SCC "schedule.Evaulate" #-} do
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

    Say msg k ->
      {-# SCC "schedule.Say" #-} do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventSay msg) trace)

    Output x k ->
      {-# SCC "schedule.Output" #-} do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventLog x) trace)

    LiftST st k ->
      {-# SCC "schedule.LiftST" #-} do
      x <- strictToLazyST st
      let thread' = thread { threadControl = ThreadControl (k x) ctl }
      schedule thread' simstate

    GetMonoTime k ->
      {-# SCC "schedule.GetMonoTime" #-} do
      let thread' = thread { threadControl = ThreadControl (k time) ctl }
      schedule thread' simstate

    GetWallTime k ->
      {-# SCC "schedule.GetWallTime" #-} do
      let !clockid  = threadClockId thread
          !clockoff = clocks Map.! clockid
          !walltime = timeSinceEpoch time `addUTCTime` clockoff
          !thread'  = thread { threadControl = ThreadControl (k walltime) ctl }
      schedule thread' simstate

    SetWallTime walltime' k ->
      {-# SCC "schedule.SetWallTime" #-} do
      let !clockid   = threadClockId thread
          !clockoff  = clocks Map.! clockid
          !walltime  = timeSinceEpoch time `addUTCTime` clockoff
          !clockoff' = addUTCTime (diffUTCTime walltime' walltime) clockoff
          !thread'   = thread { threadControl = ThreadControl k ctl }
          !simstate' = simstate { clocks = Map.insert clockid clockoff' clocks }
      schedule thread' simstate'

    UnshareClock k ->
      {-# SCC "schedule.UnshareClock" #-} do
      let !clockid   = threadClockId thread
          !clockoff  = clocks Map.! clockid
          !clockid'  = let ThreadId i = tid in ClockId i -- reuse the thread id
          !thread'   = thread { threadControl = ThreadControl k ctl
                              , threadClockId = clockid' }
          !simstate' = simstate { clocks = Map.insert clockid' clockoff clocks }
      schedule thread' simstate'

    -- This case is guarded by checks in 'timeout' itself.
    StartTimeout d _ _ | d <= 0 ->
      error "schedule: StartTimeout: Impossible happened"

    StartTimeout d action' k ->
      {-# SCC "schedule.StartTimeout" #-} do
      isLockedRef <- newSTRef NotLocked
      let !expiry    = d `addTime` time
          !timers'   = PSQ.insert nextTmid expiry (TimerTimeout tid nextTmid isLockedRef) timers
          !thread'   = thread { threadControl =
                                 ThreadControl action'
                                               (TimeoutFrame nextTmid isLockedRef k ctl)
                              }
      !trace <- deschedule Yield thread' simstate { timers   = timers'
                                                  , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimeoutCreated nextTmid tid expiry) trace)

    RegisterDelay d k | d < 0 ->
      {-# SCC "schedule.NewRegisterDelay.1" #-} do
      !tvar <- execNewTVar nextVid
                          (Just $ "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                          True
      let !expiry  = d `addTime` time
          !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
      trace <- schedule thread' simstate { nextVid = succ nextVid }
      return (SimTrace time tid tlbl (EventRegisterDelayCreated nextTmid nextVid expiry) $
              SimTrace time tid tlbl (EventRegisterDelayFired nextTmid) $
              trace)

    RegisterDelay d k ->
      {-# SCC "schedule.NewRegisterDelay.2" #-} do
      !tvar <- execNewTVar nextVid
                          (Just $ "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                          False
      let !expiry  = d `addTime` time
          !timers' = PSQ.insert nextTmid expiry (TimerRegisterDelay tvar) timers
          !thread' = thread { threadControl = ThreadControl (k tvar) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl
                (EventRegisterDelayCreated nextTmid nextVid expiry) trace)

    ThreadDelay d k | d < 0 ->
      {-# SCC "schedule.NewThreadDelay" #-} do
      let !expiry    = d `addTime` time
          !thread'   = thread { threadControl = ThreadControl k ctl }
          !simstate' = simstate { nextTmid = succ nextTmid }
      trace <- schedule thread' simstate'
      return (SimTrace time tid tlbl (EventThreadDelay nextTmid expiry) $
              SimTrace time tid tlbl (EventThreadDelayFired nextTmid) $
              trace)

    ThreadDelay d k ->
      {-# SCC "schedule.NewThreadDelay" #-} do
      let !expiry  = d `addTime` time
          !timers' = PSQ.insert nextTmid expiry (TimerThreadDelay tid nextTmid) timers
          !thread' = thread { threadControl = ThreadControl k ctl }
      !trace <- deschedule (Blocked BlockedOnOther) thread' simstate { timers   = timers'
                                                                     , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventThreadDelay nextTmid expiry) trace)

    -- we treat negative timers as cancelled ones; for the record we put
    -- `EventTimerCreated` and `EventTimerCancelled` in the trace; This differs
    -- from `GHC.Event` behaviour.
    NewTimeout d k | d < 0 ->
      {-# SCC "schedule.NewTimeout.1" #-} do
      let !t       = NegativeTimeout nextTmid
          !expiry  = d `addTime` time
          !thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimerCreated nextTmid nextVid expiry) $
              SimTrace time tid tlbl (EventTimerCancelled nextTmid) $
              trace)

    NewTimeout d k ->
      {-# SCC "schedule.NewTimeout.2" #-} do
      !tvar  <- execNewTVar nextVid
                           (Just $ "<<timeout-state " ++ show (unTimeoutId nextTmid) ++ ">>")
                           TimeoutPending
      let !expiry  = d `addTime` time
          !t       = Timeout tvar nextTmid
          !timers' = PSQ.insert nextTmid expiry (Timer tvar) timers
          !thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimerCreated nextTmid nextVid expiry) trace)

    -- we do not follow `GHC.Event` behaviour here; updating a timer to the past
    -- effectively cancels it.
    UpdateTimeout (Timeout _tvar tmid) d k | d < 0 ->
      {-# SCC "schedule.UpdateTimeout" #-} do
      let !timers' = PSQ.delete tmid timers
          !thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimTrace time tid tlbl (EventTimerCancelled tmid) trace)

    UpdateTimeout (Timeout _tvar tmid) d k ->
      {-# SCC "schedule.UpdateTimeout" #-} do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimeout_  Nothing       = ((), Nothing)
          updateTimeout_ (Just (_p, v)) = ((), Just (expiry, v))
          !expiry  = d `addTime` time
          !timers' = snd (PSQ.alter updateTimeout_ tmid timers)
          !thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimTrace time tid tlbl (EventTimerUpdated tmid expiry) trace)

    -- updating a negative timer is a no-op, unlike in `GHC.Event`.
    UpdateTimeout (NegativeTimeout _tmid) _d k ->
      {-# SCC "schedule.UpdateTimeout" #-} do
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    CancelTimeout (Timeout tvar tmid) k ->
      {-# SCC "schedule.CancelTimeout" #-} do
      let !timers' = PSQ.delete tmid timers
          !thread' = thread { threadControl = ThreadControl k ctl }
      !written <- execAtomically' (runSTM $ writeTVar tvar TimeoutCancelled)
      (wakeup, wokeby) <- threadsUnblockedByWrites written
      mapM_ (\(SomeTVar var) -> unblockAllThreadsFromTVar var) written
      let (unblocked,
           simstate') = unblockThreads True wakeup simstate
      trace <- schedule thread' simstate' { timers = timers' }
      return $ SimTrace time tid tlbl (EventTimerCancelled tmid)
             $ traceMany
                 [ (time, tid', tlbl', EventTxWakeup vids)
                 | tid' <- unblocked
                 , let tlbl' = lookupThreadLabel tid' threads
                 , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
             $ trace

    -- cancelling a negative timer is a no-op
    CancelTimeout (NegativeTimeout _tmid) k ->
      {-# SCC "schedule.CancelTimeout" #-} do
      -- negative timers are promptly removed from the state
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    Fork a k ->
      {-# SCC "schedule.Fork" #-} do
      let !nextId   = threadNextTId thread
          !tid'     = childThreadId tid nextId
          !thread'  = thread { threadControl = ThreadControl (k tid') ctl
                             , threadNextTId = succ nextId }
          !thread'' = Thread { threadId      = tid'
                             , threadControl = ThreadControl (runIOSim a)
                                                             ForkFrame
                             , threadStatus  = ThreadRunning
                             , threadMasking = threadMasking thread
                             , threadThrowTo = []
                             , threadClockId = threadClockId thread
                             , threadLabel   = Nothing
                             , threadNextTId = 1
                             }
          !threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = Deque.snoc tid' runqueue
                                         , threads  = threads' }
      return (SimTrace time tid tlbl (EventThreadForked tid') trace)

    Atomically a k ->
      {-# SCC "schedule.Atomically" #-} execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
      case res of
        StmTxCommitted x written _read created
                         tvarDynamicTraces tvarStringTraces nextVid' -> do
          (!wakeup, wokeby) <- threadsUnblockedByWrites written
          !_ <- mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          let thread'     = thread { threadControl = ThreadControl (k x) ctl }
              (unblocked,
               simstate') = unblockThreads True wakeup simstate
          written' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
          created' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) created
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
          !trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $ SimTrace time tid tlbl (EventTxCommitted
                                             written' created' Nothing)
                 $ traceMany
                     [ (time, tid', tlbl', EventTxWakeup vids')
                     | tid' <- unblocked
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ]
                 $ traceMany
                     [ (time, tid, tlbl, EventLog tr)
                     | tr <- tvarDynamicTraces ]
                 $ traceMany
                     [ (time, tid, tlbl, EventSay str)
                     | str <- tvarStringTraces ]
                 $ SimTrace time tid tlbl (EventUnblocked unblocked)
                 $ SimTrace time tid tlbl (EventDeschedule Yield)
                 $ trace

        StmTxAborted _read e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw ThrowSelf e) ctl }
          !trace <- schedule thread' simstate
          return $ SimTrace time tid tlbl (EventTxAborted Nothing) trace

        StmTxBlocked read -> do
          !_ <- mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
          !trace <- deschedule (Blocked BlockedOnSTM) thread simstate
          return $ SimTrace time tid tlbl (EventTxBlocked vids Nothing)
                 $ SimTrace time tid tlbl (EventDeschedule (Blocked BlockedOnSTM))
                 $ trace

    GetThreadId k ->
      {-# SCC "schedule.GetThreadId" #-} do
      let thread' = thread { threadControl = ThreadControl (k tid) ctl }
      schedule thread' simstate

    LabelThread tid' l k | tid' == tid ->
      {-# SCC "schedule.LabelThread" #-} do
      let thread' = thread { threadControl = ThreadControl k ctl
                           , threadLabel   = Just l }
      schedule thread' simstate

    LabelThread tid' l k ->
      {-# SCC "schedule.LabelThread" #-} do
      let thread'  = thread { threadControl = ThreadControl k ctl }
          threads' = Map.adjust (\t -> t { threadLabel = Just l }) tid' threads
      schedule thread' simstate { threads = threads' }

    ThreadStatus tid' k ->
      {-# SCC "schedule.ThreadStatus" #-} do
      let result | Just r <- Map.lookup tid' finished = reasonToStatus r
                 | Just t <- Map.lookup tid' threads  = ghcThreadStatus (threadStatus t)
                 | otherwise                          = error "The impossible happened - tried to loookup thread in state."
          reasonToStatus FinishedNormally    = GHC.ThreadFinished
          reasonToStatus FinishedDied        = GHC.ThreadDied

          thread' = thread { threadControl = ThreadControl (k result) ctl }
      schedule thread' simstate

    GetMaskState k ->
      {-# SCC "schedule.GetMaskState" #-} do
      let thread' = thread { threadControl = ThreadControl (k maskst) ctl }
      schedule thread' simstate

    SetMaskState maskst' action' k ->
      {-# SCC "schedule.SetMaskState" #-} do
      let thread' = thread { threadControl = ThreadControl
                                               (runIOSim action')
                                               (MaskFrame k maskst ctl)
                           , threadMasking = maskst' }
      trace <-
        case maskst' of
          -- If we're now unmasked then check for any pending async exceptions
          Unmasked -> SimTrace time tid tlbl (EventDeschedule Interruptable)
                  <$> deschedule Interruptable thread' simstate
          _        -> schedule                 thread' simstate
      return $ SimTrace time tid tlbl (EventMask maskst')
             $ trace

    ThrowTo e tid' _ | tid' == tid ->
      {-# SCC "schedule.ThrowTo" #-} do
      -- Throw to ourself is equivalent to a synchronous throw,
      -- and works irrespective of masking state since it does not block.
      let thread' = thread { threadControl = ThreadControl (Throw ThrowSelf e) ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventThrowTo e tid) trace)

    ThrowTo e tid' k ->
      {-# SCC "schedule.ThrowTo" #-} do
      let thread'   = thread { threadControl = ThreadControl k ctl }
          willBlock = case Map.lookup tid' threads of
                        Just t -> not (threadInterruptible t)
                        _      -> False
      if willBlock
        then do
          -- The target thread has async exceptions masked so we add the
          -- exception and the source thread id to the pending async exceptions.
          let adjustTarget t = t { threadThrowTo = (e, Labelled tid tlbl) : threadThrowTo t }
              threads'       = Map.adjust adjustTarget tid' threads
          !trace <- deschedule (Blocked BlockedOnOther) thread' simstate { threads = threads' }
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ SimTrace time tid tlbl EventThrowToBlocked
                 $ SimTrace time tid tlbl (EventDeschedule (Blocked BlockedOnOther))
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
          let thrower = case threadMasking <$> Map.lookup tid' threads of
                          Just Unmasked -> ThrowOther
                          _             -> ThrowSelf
              adjustTarget t@Thread{ threadControl = ThreadControl _ ctl' } =
                t { threadControl = ThreadControl (Throw thrower e) ctl'
                  , threadStatus  = ThreadRunning
                  }
              simstate'@SimState { threads = threads' }
                         = snd (unblockThreads False [tid'] simstate)
              threads''  = Map.adjust adjustTarget tid' threads'
              simstate'' = simstate' { threads = threads'' }

          trace <- schedule thread' simstate''
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ trace

    YieldSim k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      deschedule Yield thread' simstate

    -- ExploreRaces is ignored by this simulator
    ExploreRaces k ->
      {-# SCC "schedule.ExploreRaces" #-}
      schedule thread{ threadControl = ThreadControl k ctl } simstate

    Fix f k ->
      {-# SCC "schedule.Fix" #-} do
      r <- newSTRef (throw NonTermination)
      x <- unsafeInterleaveST $ readSTRef r
      let k' = unIOSim (f x) $ \x' ->
                  LiftST (lazyToStrictST (writeSTRef r x')) (\() -> k x')
          thread' = thread { threadControl = ThreadControl k' ctl }
      schedule thread' simstate


threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                   -> True
      MaskedInterruptible
        | isThreadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise              -> False
      MaskedUninterruptible      -> False

deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (SimTrace a)
deschedule Yield !thread !simstate@SimState{runqueue, threads} =

    -- We don't interrupt runnable threads to provide fairness anywhere else.
    -- We do it here by putting the thread to the back of the runqueue, behind
    -- all other runnable threads.
    --
    -- For testing, we should have a more sophisticated policy to show that
    -- algorithms are not sensitive to the exact policy, so long as it is a
    -- fair policy (all runnable threads eventually run).

    {-# SCC "deschedule.Yield" #-}
    let runqueue' = Deque.snoc (threadId thread) runqueue
        threads'  = Map.insert (threadId thread) thread threads in
    reschedule simstate { runqueue = runqueue', threads  = threads' }

deschedule Interruptable !thread@Thread {
                           threadId      = tid,
                           threadControl = ThreadControl _ ctl,
                           threadMasking = Unmasked,
                           threadThrowTo = (e, tid') : etids,
                           threadLabel   = tlbl
                         }
                         !simstate@SimState{ curTime = time, threads } =

    -- We're unmasking, but there are pending blocked async exceptions.
    -- So immediately raise the exception and unblock the blocked thread
    -- if possible.
    {-# SCC "deschedule.Interruptable.Unmasked" #-}
    let thread' = thread { threadControl = ThreadControl (Throw ThrowSelf e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids }
        (unblocked,
         simstate') = unblockThreads False [l_labelled tid'] simstate
    in do
    trace <- schedule thread' simstate'
    return $ SimTrace time tid tlbl (EventThrowToUnmasked tid')
           $ traceMany [ (time, tid'', tlbl'', EventThrowToWakeup)
                       | tid'' <- unblocked
                       , let tlbl'' = lookupThreadLabel tid'' threads ]
             trace

deschedule Interruptable !thread !simstate =
    -- Either masked or unmasked but no pending async exceptions.
    -- Either way, just carry on.
    {-# SCC "deschedule.Interruptable.Masked" #-}
    schedule thread simstate

deschedule (Blocked _blockedReason) !thread@Thread { threadThrowTo = _ : _
                                                   , threadMasking = maskst } !simstate
    | maskst /= MaskedUninterruptible =
    -- We're doing a blocking operation, which is an interrupt point even if
    -- we have async exceptions masked, and there are pending blocked async
    -- exceptions. So immediately raise the exception and unblock the blocked
    -- thread if possible.
    {-# SCC "deschedule.Interruptable.Blocked.1" #-}
    deschedule Interruptable thread { threadMasking = Unmasked } simstate

deschedule (Blocked blockedReason) !thread !simstate@SimState{threads} =
    {-# SCC "deschedule.Interruptable.Blocked.2" #-}
    let thread'  = thread { threadStatus = ThreadBlocked blockedReason }
        threads' = Map.insert (threadId thread') thread' threads in
    reschedule simstate { threads = threads' }

deschedule (Terminated reason) !thread !simstate@SimState{ curTime = time, threads } =
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    {-# SCC "deschedule.Terminated" #-}
    let !wakeup      = map (l_labelled . snd) (reverse (threadThrowTo thread))
        (unblocked,
         !simstate') = unblockThreads False wakeup
                        simstate { finished = Map.insert (threadId thread)
                                                         reason
                                                         (finished simstate) }
    in do
    !trace <- reschedule simstate'
    return $ traceMany
               [ (time, tid', tlbl', EventThrowToWakeup)
               | tid' <- unblocked
               , let tlbl' = lookupThreadLabel tid' threads ]
               trace

deschedule Sleep _thread _simstate =
    error "IOSim: impossible happend"

-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule :: SimState s a -> ST s (SimTrace a)
reschedule !simstate@SimState{ runqueue, threads }
  | Just (!tid, runqueue') <- Deque.uncons runqueue =
    {-# SCC "reschedule.Just" #-}
    let thread = threads Map.! tid in
    schedule thread simstate { runqueue = runqueue'
                             , threads  = Map.delete tid threads }

-- But when there are no runnable threads, we advance the time to the next
-- timer event, or stop.
reschedule !simstate@SimState{ threads, timers, curTime = time } =
    {-# SCC "reschedule.Nothing" #-}

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (TraceDeadlock time (labelledThreads threads))

      Just (tmids, !time', !fired, !timers') -> assert (time' >= time) $ do
        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        !written <- execAtomically' (runSTM $ mapM_ timeoutSTMAction fired)
        (wakeupSTM, wokeby) <- threadsUnblockedByWrites written
        !_ <- mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

            -- Check all fired threadDelays
        let wakeupThreadDelay = [ (tid, tmid) | TimerThreadDelay tid tmid <- fired ]
            wakeup            = fst `fmap` wakeupThreadDelay ++ wakeupSTM
            (_, !simstate')   = unblockThreads False wakeup simstate

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

        !simstate'' <- forkTimeoutInterruptThreads timeoutExpired' simstate'

        !trace <- reschedule simstate'' { curTime = time'
                                        , timers  = timers' }

        return $
          traceMany ([ ( time', ThreadId [-1], Just "timer"
                       , EventTimerFired tmid)
                     | (tmid, Timer _) <- zip tmids fired ]
                  ++ [ ( time', ThreadId [-1], Just "register delay timer"
                       , EventRegisterDelayFired tmid)
                     | (tmid, TimerRegisterDelay _) <- zip tmids fired ]
                  ++ [ (time', tid', tlbl', EventTxWakeup vids)
                     | tid' <- wakeupSTM
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
                  ++ [ ( time', tid, Just "thread delay timer"
                       , EventThreadDelayFired tmid)
                     | (tid, tmid) <- wakeupThreadDelay ]
                  ++ [ ( time', tid, Just "timeout timer"
                       , EventTimeoutFired tmid)
                     | (tid, tmid, _, _) <- timeoutExpired' ]
                  ++ [ ( time', tid, Just "thread forked"
                       , EventThreadForked tid)
                     | (tid, _, _, _) <- timeoutExpired' ])
                    trace
  where
    timeoutSTMAction (Timer var) = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()
    timeoutSTMAction (TimerRegisterDelay var) = writeTVar var True
    -- Note that 'threadDelay' is not handled via STM style wakeup, but rather
    -- it's handled directly above with 'wakeupThreadDelay' and 'unblockThreads'
    timeoutSTMAction TimerThreadDelay{}       = return ()
    timeoutSTMAction TimerTimeout{}           = return ()

unblockThreads :: Bool -> [ThreadId] -> SimState s a -> ([ThreadId], SimState s a)
unblockThreads !onlySTM !wakeup !simstate@SimState {runqueue, threads} =
    -- To preserve our invariants (that threadStatus is correct)
    -- we update the runqueue and threads together here
    (unblocked, simstate {
                  runqueue = runqueue <> fromList unblocked,
                  threads  = threads'
                })
  where
    -- can only unblock if the thread exists and is blocked (not running)
    !unblocked = [ tid
                 | tid <- wakeup
                 , case Map.lookup tid threads of
                    Just Thread { threadStatus = ThreadBlocked BlockedOnOther }
                      -> not onlySTM
                    Just Thread { threadStatus = ThreadBlocked BlockedOnSTM }
                      -> True
                    _ -> False
                 ]
    -- and in which case we mark them as now running
    !threads'  = List.foldl'
                   (flip (Map.adjust (\t -> t { threadStatus = ThreadRunning })))
                   threads
                   unblocked

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
forkTimeoutInterruptThreads :: forall s a.
                               [(ThreadId, TimeoutId, STRef s IsLocked, IsLocked)]
                            -> SimState s a
                            -> ST s (SimState s a)
forkTimeoutInterruptThreads timeoutExpired simState =
  foldlM (\st@SimState{ runqueue = runqueue,
                        threads  = threads'
                      }
           (t, isLockedRef)
          -> do
            let threads'' = Map.insert (threadId t) t
                          $ threads'
                runqueue' = Deque.snoc (threadId t) runqueue

            writeSTRef isLockedRef (Locked (threadId t))

            return st { runqueue = runqueue',
                        threads  = threads''
                      })
          simState'
          throwToThread

  where
    -- can only throw exception if the thread exists and if the mutually
    -- exclusive lock exists and is still 'NotLocked'
    toThrow :: [(ThreadId, TimeoutId, STRef s IsLocked)]
    toThrow  = [ (tid, tmid, ref)
               | (tid, tmid, ref, NotLocked) <- timeoutExpired ]

    -- we launch a thread responsible for throwing an AsyncCancelled exception
    -- to the thread which timeout expired
    throwToThread :: [(Thread s a, STRef s IsLocked)] 

    (simState', throwToThread) = List.mapAccumR fn simState toThrow
      where
        fn :: SimState s a
           -> (ThreadId, TimeoutId, STRef s IsLocked)
           -> (SimState s a, (Thread s a, STRef s IsLocked))
        fn state@SimState { threads } (tid, tmid, ref) =
          let t = case tid `Map.lookup` threads of
                    Just t' -> t'
                    Nothing -> error ("IOSim: internal error: unknown thread " ++ show tid)
              nextId   = threadNextTId t
          in ( state { threads = Map.insert tid t { threadNextTId = succ nextId } threads }
             , ( Thread { threadId      = childThreadId tid nextId,
                          threadControl =
                            ThreadControl
                             (ThrowTo (toException (TimeoutException tmid))
                                      tid
                                      (Return ()))
                             ForkFrame,
                          threadStatus  = ThreadRunning,
                          threadMasking = Unmasked,
                          threadThrowTo = [],
                          threadClockId = threadClockId t,
                          threadLabel   = Just "timeout-forked-thread",
                          threadNextTId = 1
                        }
               , ref )
             )

-- | Iterate through the control stack to find an enclosing exception handler
-- of the right type, or unwind all the way to the top level for the thread.
--
-- Also return if it's the main thread or a forked thread since we handle the
-- cases differently.
--
-- Also remove timeouts associated to frames we unwind.
--
unwindControlStack :: forall s a.
                      SomeException
                   -> Thread s a
                   -> Timeouts s
                   -> ( Either Bool (Thread s a)
                      , Timeouts s
                      )
unwindControlStack e thread timers0 =
    case threadControl thread of
      ThreadControl _ ctl -> unwind (threadMasking thread) ctl timers0
  where
    unwind :: forall s' c. MaskingState
           -> ControlStack s' c a
           -> OrdPSQ TimeoutId Time (TimerCompletionInfo s)
           -> (Either Bool (Thread s' a), OrdPSQ TimeoutId Time (TimerCompletionInfo s))
    unwind _  MainFrame                 timers = (Left True, timers)
    unwind _  ForkFrame                 timers = (Left False, timers)
    unwind _ (MaskFrame _k maskst' ctl) timers = unwind maskst' ctl timers

    unwind maskst (CatchFrame handler k ctl) timers =
      case fromException e of
        -- not the right type, unwind to the next containing handler
        Nothing -> unwind maskst ctl timers

        -- Ok! We will be able to continue the thread with the handler
        -- followed by the continuation after the catch
        Just e' -> ( Right thread {
                              -- As per async exception rules, the handler is run
                              -- masked
                             threadControl = ThreadControl (handler e')
                                                           (MaskFrame k maskst ctl),
                             threadMasking = atLeastInterruptibleMask maskst
                           }
                   , timers
                   )

    -- Either Timeout fired or the action threw an exception.
    -- - If Timeout fired, then it was possibly during this thread's execution
    --   so we need to run the continuation with a Nothing value.
    -- - If the timeout action threw an exception we need to keep unwinding the
    --   control stack looking for a handler to this exception.
    unwind maskst (TimeoutFrame tmid _ k ctl) timers =
        case fromException e of
          -- Exception came from timeout expiring
          Just (TimeoutException tmid') | tmid == tmid' ->
            (Right thread { threadControl = ThreadControl (k Nothing) ctl }, timers')
          -- Exception came from a different exception
          _ -> unwind maskst ctl timers'
      where
        -- Remove the timeout associated with the 'TimeoutFrame'.
        timers' = PSQ.delete tmid timers

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
    collectAll !ks !p !xs !psq =
      case PSQ.minView psq of
        Just (k, p', x, psq')
          | p == p' -> collectAll (k:ks) p (x:xs) psq'
        _           -> (reverse ks, p, reverse xs, psq)

traceMany :: [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
          -> SimTrace a -> SimTrace a
traceMany []                      trace = trace
traceMany ((time, tid, tlbl, event):ts) trace =
    SimTrace time tid tlbl event (traceMany ts trace)

lookupThreadLabel :: ThreadId -> Map ThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in 'ST' monad.  One can
-- recover failures or the result from 'SimTrace' with 'traceResult', or access
-- 'SimEventType's generated by the computation with 'traceEvents'.  A slightly
-- more convenient way is exposed by 'runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (SimTrace a)
runSimTraceST mainAction = schedule mainThread initialState
  where
    mainThread =
      Thread {
        threadId      = ThreadId [],
        threadControl = ThreadControl (runIOSim mainAction) MainFrame,
        threadStatus  = ThreadRunning,
        threadMasking = Unmasked,
        threadThrowTo = [],
        threadClockId = ClockId [],
        threadLabel   = Just "main",
        threadNextTId = 1
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
execAtomically !time !tid !tlbl !nextVid0 action0 k0 =
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
                                []
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
          -- Skip the right hand alternative and continue with the k continuation
          go ctl' read written' writtenSeq' createdSeq' nextVid (k x)

      ThrowStm e ->
        {-# SCC "execAtomically.go.ThrowStm" #-} do
        -- Rollback `TVar`s written since catch handler was installed
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
        -- Execute the catch handler with an empty written set.
        -- but preserve ones that were set prior to it, as specified in the
        -- [stm](https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#v:catchSTM) package.
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
        go ctl read written writtenSeq (SomeTVar v : createdSeq) (succ nextVid) (k v)

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
        return $ SimTrace time tid tlbl (EventSay msg) trace

      OutputStm x k ->
        {-# SCC "execAtomically.go.OutputStm" #-} do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        return $ SimTrace time tid tlbl (EventLog x) trace

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
         == Set.fromList [ tvarId tvar | SomeTVar tvar <- writtenSeq ]


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
    !tvarLabel   <- newSTRef mbLabel
    !tvarCurrent <- newSTRef x
    !tvarUndo    <- newSTRef $! []
    !tvarBlocked <- newSTRef ([], Set.empty)
    !tvarVClock  <- newSTRef $! VectorClock Map.empty
    !tvarTrace   <- newSTRef $! Nothing
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
    !_ <- writeSTRef tvarUndo (v:vs)
    return ()

revertTVar :: TVar s a -> ST s ()
revertTVar TVar{tvarCurrent, tvarUndo} = do
    -- pop the undo stack, and revert the current value
    vs <- readSTRef tvarUndo
    !_ <- writeSTRef tvarCurrent (head vs)
    !_ <- writeSTRef tvarUndo    (tail vs)
    return ()
{-# INLINE revertTVar #-}

commitTVar :: TVar s a -> ST s ()
commitTVar TVar{tvarUndo} = do
    vs <- readSTRef tvarUndo
    -- pop the undo stack, leaving the current value unchanged
    !_ <- writeSTRef tvarUndo (tail vs)
    return ()
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
      Nothing -> return TraceValue { traceDynamic = (Nothing :: Maybe ())
                                   , traceString = Nothing }
      Just f  -> do
        vs <- readSTRef tvarUndo
        v  <- readSTRef tvarCurrent
        case (new, vs) of
          (True, _) -> f Nothing v
          (_, _:_)  -> f (Just $ last vs) v
          _         -> error "traceTVarST: unexpected tvar state"



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
      !_ <- writeSTRef tvarBlocked (tids', tidsSet')
      return ()

unblockAllThreadsFromTVar :: TVar s a -> ST s ()
unblockAllThreadsFromTVar TVar{tvarBlocked} = do
    !_ <- writeSTRef tvarBlocked ([], Set.empty)
    return ()

-- | For each TVar written to in a transaction (in order) collect the threads
-- that blocked on each one (in order).
--
-- Also, for logging purposes, return an association between the threads and
-- the var writes that woke them.
--
threadsUnblockedByWrites :: [SomeTVar s]
                         -> ST s ([ThreadId], Map ThreadId (Set (Labelled TVarId)))
threadsUnblockedByWrites written = do
  !tidss <- sequence
             [ (,) <$> labelledTVarId tvar <*> readTVarBlockedThreads tvar
             | SomeTVar tvar <- written ]
  -- Threads to wake up, in wake up order, annotated with the vars written that
  -- caused the unblocking.
  -- We reverse the individual lists because the tvarBlocked is used as a stack
  -- so it is in order of last written, LIFO, and we want FIFO behaviour.
  let !wakeup = ordNub [ tid | (_vid, tids) <- tidss, tid <- reverse tids ]
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
{-# INLINE ordNub #-}
