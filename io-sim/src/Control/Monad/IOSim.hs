{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Control.Monad.IOSim
  ( -- * Simulation monad
    IOSim
  , STMSim
    -- ** Run simulation
  , runSim
  , runSimOrThrow
  , runSimStrictShutdown
  , Failure (..)
  , runSimTrace
  , runSimTraceST
    -- ** Explore races using /IOSimPOR/
    -- $iosimpor
  , exploreSimTrace
  , controlSimTrace
  , ScheduleMod (..)
  , ScheduleControl (..)
    -- *** Exploration options
  , ExplorationSpec
  , ExplorationOptions (..)
  , stdExplorationOptions
  , withScheduleBound
  , withBranching
  , withStepTimelimit
  , withReplay
    -- * Lift ST computations
  , liftST
    -- * Simulation time
  , setCurrentTime
  , unshareClock
    -- * Simulation trace
  , type SimTrace
  , Trace (Cons, Nil, SimTrace, SimPORTrace, TraceDeadlock, TraceLoop, TraceMainReturn, TraceMainException, TraceRacesFound)
  , SimResult (..)
  , SimEvent (..)
  , SimEventType (..)
  , ThreadLabel
  , Labelled (..)
    -- ** Dynamic Tracing
  , traceM
  , traceSTM
    -- ** Pretty printers
  , ppTrace
  , ppTrace_
  , ppEvents
  , ppSimEvent
  , ppDebug
    -- ** Selectors
  , traceEvents
  , traceResult
    -- *** list selectors
  , selectTraceEvents
  , selectTraceEvents'
  , selectTraceEventsDynamic
  , selectTraceEventsDynamicWithTime
  , selectTraceEventsDynamic'
  , selectTraceEventsDynamicWithTime'
  , selectTraceEventsSay
  , selectTraceEventsSayWithTime
  , selectTraceEventsSay'
  , selectTraceEventsSayWithTime'
  , selectTraceRaces
    -- *** trace selectors
  , traceSelectTraceEvents
  , traceSelectTraceEventsDynamic
  , traceSelectTraceEventsSay
    -- ** IO printer
  , printTraceEventsSay
    -- * Eventlog
  , EventlogEvent (..)
  , EventlogMarker (..)
    -- * Low-level API
  , newTimeout
  , readTimeout
  , cancelTimeout
  , awaitTimeout
  ) where

import           Prelude

import           Data.Bifoldable
import           Data.Dynamic (fromDynamic)
import           Data.List (intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           Data.List.Trace (Trace (..))

import           Control.Exception (throw)

import           Control.Monad.ST.Lazy

import           Control.Monad.Class.MonadThrow as MonadThrow

import           Control.Monad.IOSim.Internal (runSimTraceST)
import           Control.Monad.IOSim.Types
import           Control.Monad.IOSimPOR.Internal (controlSimTraceST)
import           Control.Monad.IOSimPOR.QuickCheckUtils

import           Test.QuickCheck


import           Data.IORef
import           System.IO.Unsafe


selectTraceEvents
    :: (Time -> SimEventType -> Maybe b)
    -> SimTrace a
    -> [b]
selectTraceEvents fn =
      bifoldr ( \ v _
               -> case v of
                    MainException _ e _       -> throw (FailureException e)
                    Deadlock      _   threads -> throw (FailureDeadlock threads)
                    MainReturn    _ _ _       -> []
                    Loop                      -> error "Impossible: selectTraceEvents _ TraceLoop{}"
              )
              ( \ b acc -> b : acc )
              []
    . traceSelectTraceEvents fn

selectTraceEvents'
    :: (Time ->  SimEventType -> Maybe b)
    -> SimTrace a
    -> [b]
selectTraceEvents' fn =
      bifoldr ( \ _ _   -> []  )
              ( \ b acc -> b : acc )
              []
    . traceSelectTraceEvents fn

selectTraceRaces :: SimTrace a -> [ScheduleControl]
selectTraceRaces = go
  where
    go (SimTrace _ _ _ _ trace)      = go trace
    go (SimPORTrace _ _ _ _ _ trace) = go trace
    go (TraceRacesFound races trace) =
      races ++ go trace
    go _                             = []

-- Extracting races from a trace.  There is a subtlety in doing so: we
-- must return a defined list of races even in the case where the
-- trace is infinite, and there are no races occurring in it! For
-- example, if the system falls into a deterministic infinite loop,
-- then there will be no races to find.

-- In reality we only want to extract races from *the part of the
-- trace used in a test*. We can only observe that by tracking lazy
-- evaluation: only races that were found in the evaluated prefix of
-- an infinite trace should contribute to the "races found". Hence we
-- return a function that returns the races found "so far". This is
-- unsafe, of course, since that function may return different results
-- at different times.

detachTraceRaces :: forall a. SimTrace a -> (() -> [ScheduleControl], SimTrace a)
detachTraceRaces trace = unsafePerformIO $ do
  races <- newIORef []
  let readRaces :: () -> [ScheduleControl]
      readRaces () = concat . reverse . unsafePerformIO $ readIORef races

      saveRaces :: [ScheduleControl] -> x -> x
      saveRaces rs t = unsafePerformIO $ modifyIORef races (rs:)
                                      >> return t

      go :: SimTrace a -> SimTrace a
      go (SimTrace a b c d trace)      = SimTrace a b c d $ go trace
      go (SimPORTrace a b c d e trace) = SimPORTrace a b c d e $ go trace
      go (TraceRacesFound rs trace)    = saveRaces rs $ go trace
      go t                             = t

  return (readRaces, go trace)

-- | Select all the traced values matching the expected type. This relies on
-- the sim's dynamic trace facility.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceEventsDynamic :: forall a b. Typeable b => SimTrace a -> [b]
selectTraceEventsDynamic = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe b
    fn _ (EventLog dyn) = fromDynamic dyn
    fn _ _              = Nothing

-- | Like 'selectTraceEventsDynamic' but also captures time of the trace event.
--
selectTraceEventsDynamicWithTime :: forall a b. Typeable b => SimTrace a -> [(Time, b)]
selectTraceEventsDynamicWithTime = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, b)
    fn t (EventLog dyn) = (t,) <$> fromDynamic dyn
    fn _ _              = Nothing

-- | Like 'selectTraceEventsDynamic' but returns partial trace if an exception
-- is found in it.
--
selectTraceEventsDynamic' :: forall a b. Typeable b => SimTrace a -> [b]
selectTraceEventsDynamic' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe b
    fn _ (EventLog dyn) = fromDynamic dyn
    fn _ _              = Nothing

-- | Like `selectTraceEventsDynamic'` but also captures time of the trace event.
--
selectTraceEventsDynamicWithTime' :: forall a b. Typeable b => SimTrace a -> [(Time, b)]
selectTraceEventsDynamicWithTime' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, b)
    fn t (EventLog dyn) = (t,) <$> fromDynamic dyn
    fn _ _              = Nothing

-- | Get a trace of 'EventSay'.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceEventsSay :: SimTrace a -> [String]
selectTraceEventsSay = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe String
    fn _ (EventSay s) = Just s
    fn _ _            = Nothing

-- | Like 'selectTraceEventsSay' but also captures time of the trace event.
--
selectTraceEventsSayWithTime :: SimTrace a -> [(Time, String)]
selectTraceEventsSayWithTime = selectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, String)
    fn t (EventSay s) = Just (t, s)
    fn _ _            = Nothing

-- | Like 'selectTraceEventsSay' but return partial trace if an exception is
-- found in it.
--
selectTraceEventsSay' :: SimTrace a -> [String]
selectTraceEventsSay' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe String
    fn _ (EventSay s) = Just s
    fn _ _            = Nothing

-- | Like `selectTraceEventsSay'` but also captures time of the trace event.
--
selectTraceEventsSayWithTime' :: SimTrace a -> [(Time, String)]
selectTraceEventsSayWithTime' = selectTraceEvents' fn
  where
    fn :: Time -> SimEventType -> Maybe (Time, String)
    fn t (EventSay s) = Just (t, s)
    fn _ _            = Nothing

-- | Print all 'EventSay' to the console.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
printTraceEventsSay :: SimTrace a -> IO ()
printTraceEventsSay = mapM_ print . selectTraceEventsSay


-- | The most general select function.  It is a _total_ function.
--
traceSelectTraceEvents
    :: (Time -> SimEventType -> Maybe b)
    -> SimTrace a
    -> Trace (SimResult a) b
traceSelectTraceEvents fn = bifoldr ( \ v _acc -> Nil v )
                                    ( \ eventCtx acc
                                     -> case eventCtx of
                                          SimRacesFound _ -> acc
                                          SimEvent{} ->
                                            case fn (seTime eventCtx) (seType eventCtx) of
                                              Nothing -> acc
                                              Just b  -> Cons b acc
                                          SimPOREvent{} ->
                                            case fn (seTime eventCtx) (seType eventCtx) of
                                              Nothing -> acc
                                              Just b  -> Cons b acc
                                    )
                                    undefined -- it is ignored

-- | Select dynamic events.  It is a _total_ function.
--
traceSelectTraceEventsDynamic :: forall a b. Typeable b
                              => SimTrace a -> Trace (SimResult a) b
traceSelectTraceEventsDynamic = traceSelectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe b
    fn _ (EventLog dyn) = fromDynamic dyn
    fn _ _              = Nothing


-- | Select say events.  It is a _total_ function.
--
traceSelectTraceEventsSay :: forall a.  SimTrace a -> Trace (SimResult a) String
traceSelectTraceEventsSay = traceSelectTraceEvents fn
  where
    fn :: Time -> SimEventType -> Maybe String
    fn _ (EventSay s) = Just s
    fn _ _            = Nothing

-- | Simulation terminated a failure.
--
data Failure =
       -- | The main thread terminated with an exception.
       FailureException SomeException

       -- | The threads all deadlocked.
     | FailureDeadlock ![Labelled ThreadId]

       -- | The main thread terminated normally but other threads were still
       -- alive, and strict shutdown checking was requested.
       -- See 'runSimStrictShutdown'.
     | FailureSloppyShutdown [Labelled ThreadId]

       -- | An exception was thrown while evaluation the trace.
       -- This could be an internal assertion failure of `io-sim` or an
       -- unhandled exception in the simulation.
     | FailureEvaluation SomeException
  deriving Show

instance Exception Failure where
    displayException (FailureException err) = displayException  err
    displayException (FailureDeadlock threads) =
      concat [ "<<io-sim deadlock: "
             , intercalate ", " (show `map` threads)
             , ">>"
             ]
    displayException (FailureSloppyShutdown threads) =
      concat [ "<<io-sim sloppy shutdown: "
             , intercalate ", " (show `map` threads)
             , ">>"
             ]
    displayException (FailureEvaluation err) = "evaluation error:" ++ displayException  err
    

-- | 'IOSim' is a pure monad.
--
runSim :: forall a. (forall s. IOSim s a) -> Either Failure a
runSim mainAction = traceResult False (runSimTrace mainAction)

-- | For quick experiments and tests it is often appropriate and convenient to
-- simply throw failures as exceptions.
--
runSimOrThrow :: forall a. (forall s. IOSim s a) -> a
runSimOrThrow mainAction =
    case runSim mainAction of
      Left  e -> throw e
      Right x -> x

-- | Like 'runSim' but fail when the main thread terminates if there are other
-- threads still running or blocked. If one is trying to follow a strict thread
-- cleanup policy then this helps testing for that.
--
runSimStrictShutdown :: forall a. (forall s. IOSim s a) -> Either Failure a
runSimStrictShutdown mainAction = traceResult True (runSimTrace mainAction)

-- | Fold through the trace and return either a 'Failure' or the simulation
-- result, i.e. the return value of the main thread.
--
traceResult :: Bool
            -- ^ if True the simulation will fail if there are any threads which
            -- didn't terminated when the main thread terminated.
            -> SimTrace a
            -- ^ simulation trace
            -> Either Failure a
traceResult strict = unsafePerformIO . eval
  where
    eval :: SimTrace a -> IO (Either Failure a)
    eval a = do
      r <- try (evaluate a)
      case r of
        Left e  -> return (Left (FailureEvaluation e))
        Right _ -> go a

    go :: SimTrace a -> IO (Either Failure a)
    go (SimTrace _ _ _ _ t)             = eval t
    go (SimPORTrace _ _ _ _ _ t)        = eval t
    go (TraceRacesFound _ t)            = eval t
    go (TraceMainReturn _ _ tids@(_:_))
                               | strict = pure $ Left (FailureSloppyShutdown tids)
    go (TraceMainReturn _ x _)          = pure $ Right x
    go (TraceMainException _ e _)       = pure $ Left (FailureException e)
    go (TraceDeadlock   _   threads)    = pure $ Left (FailureDeadlock threads)
    go TraceLoop{}                      = error "Impossible: traceResult TraceLoop{}"

-- | Turn 'SimTrace' into a list of timestamped events.
--
traceEvents :: SimTrace a -> [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
traceEvents (SimTrace time tid tlbl event t)      = (time, tid, tlbl, event)
                                                  : traceEvents t
traceEvents (SimPORTrace time tid _ tlbl event t) = (time, tid, tlbl, event)
                                                  : traceEvents t
traceEvents _                                     = []


-- | Pretty print a timestamped event.
--
ppEvents :: [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
         -> String
ppEvents events =
    intercalate "\n"
      [ ppSimEvent timeWidth tidWidth width
                   SimEvent {seTime, seThreadId, seThreadLabel, seType }
      | (seTime, seThreadId, seThreadLabel, seType) <- events
      ]
  where
    timeWidth = maximum
                [ length (show t)
                | (t, _, _, _) <- events
                ]
    tidWidth  = maximum
                [ length (show tid)
                | (_, tid, _, _) <- events
                ]
    width     = maximum
                [ maybe 0 length threadLabel
                | (_, _, threadLabel, _) <- events
                ]


-- | See 'runSimTraceST' below.
--
runSimTrace :: forall a. (forall s. IOSim s a) -> SimTrace a
runSimTrace mainAction = runST (runSimTraceST mainAction)

--
-- IOSimPOR
--
--
-- $iosimpor
--
-- /IOSimPOR/ is a different interpreter of 'IOSim' which has the ability to
-- discover race conditions and replay the simulation using a schedule which
-- reverts them.  For extended documentation how to use it see
-- [here](https://github.com/input-output-hk/io-sim/blob/main/io-sim/how-to-use-IOSimPOR.md).
--
-- /IOSimPOR/ only discovers races between events which happen in the same time
-- slot.  In /IOSim/ and /IOSimPOR/ time only moves explicitly through timer
-- events, e.g. things like `Control.Monad.Class.MonadTimer.SI.threadDelay`,
-- `Control.Monad.Class.MonadTimer.SI.registerDelay` or the
-- `Control.Monad.Class.MonadTimer.NonStandard.MonadTimeout` api.  The usual
-- quickcheck techniques can help explore different schedules of
-- threads too.

-- | Execute a simulation, discover & revert races.  Note that this will execute
-- the simulation multiple times with different schedules, and thus it's much
-- more costly than a simple `runSimTrace` (also the simulation environments has
-- much more state to track and hence is slower).
--
-- On property failure it will show the failing schedule (`ScheduleControl`)
-- which can be plugged to `controlSimTrace`.
--
exploreSimTrace
  :: forall a test. Testable test
  => (ExplorationOptions -> ExplorationOptions)
  -- ^ modify default exploration options
  -> (forall s. IOSim s a)
  -- ^ a simulation to run
  -> (Maybe (SimTrace a) -> SimTrace a -> test)
  -- ^ a callback which receives the previous trace (e.g. before reverting
  -- a race condition) and current trace
  -> Property
exploreSimTrace optsf mainAction k =
  case explorationReplay opts of
    Nothing ->
      explore (explorationScheduleBound opts) (explorationBranching opts) ControlDefault Nothing .&&.
      let size = cacheSize() in size `seq`
      tabulate "Modified schedules explored" [bucket size] True
    Just control ->
      replaySimTrace opts mainAction control (k Nothing)
  where
    opts = optsf stdExplorationOptions

    explore :: Int -- schedule bound
            -> Int -- branching factor
            -> ScheduleControl -> Maybe (SimTrace a) -> Property
    explore n m control passingTrace =

      -- ALERT!!! Impure code: readRaces must be called *after* we have
      -- finished with trace.
      let (readRaces, trace0) = detachTraceRaces $
                                controlSimTrace
                                  (explorationStepTimelimit opts) control mainAction
          (sleeper,trace) = compareTraces passingTrace trace0
      in ( counterexample ("Schedule control: " ++ show control)
         $ counterexample
            (case sleeper of
              Nothing -> "No thread delayed"
              Just ((t,tid,lab),racing) ->
                showThread (tid,lab) ++
                " delayed at time "++
                show t ++
                "\n  until after:\n" ++
                unlines (map (("    "++).showThread) $ Set.toList racing)
             )
         $ k passingTrace trace
         )
      .&&| let limit     = (n+m-1) `div` m
               -- To ensure the set of schedules explored is deterministic, we
               -- filter out cached ones *after* selecting the children of this
               -- node.
               races     = filter (not . cached) . take limit $ readRaces ()
               branching = length races
           in -- tabulate "Races explored" (map show races) $
              tabulate "Branching factor" [bucket branching] $
              tabulate "Race reversals per schedule" [bucket (raceReversals control)] $
              conjoinPar
                [ --Debug.trace "New schedule:" $
                  --Debug.trace ("  "++show r) $
                  --counterexample ("Schedule control: " ++ show r) $
                  explore n' ((m-1) `max` 1) r (Just trace0)
                | (r,n') <- zip races (divide (n-branching) branching) ]

    bucket :: Int -> String
    bucket n | n<10      = show n
             | otherwise = buck n 1
    buck n t | n<10      = show (n*t) ++ "-" ++ show ((n+1)*t-1)
             | otherwise = buck (n `div` 10) (t*10)

    -- divide n into k factors which sums up to n
    divide :: Int -> Int -> [Int]
    divide n k =
      [ n `div` k + if i < n `mod` k then 1 else 0
      | i <- [0..k-1] ]

    showThread :: (ThreadId,Maybe ThreadLabel) -> String
    showThread (tid,lab) =
      show tid ++ (case lab of Nothing -> ""
                               Just l  -> " ("++l++")")

    -- cache of explored schedules
    cache :: IORef (Set ScheduleControl)
    cache = unsafePerformIO cacheIO

    -- insert a schedule into the cache
    cached :: ScheduleControl -> Bool
    cached = unsafePerformIO . cachedIO

    -- compute cache size; it's a function to make sure that `GHC` does not
    -- inline it (and share the same thunk).
    cacheSize :: () -> Int
    cacheSize = unsafePerformIO . cacheSizeIO

    --
    -- Caching in IO monad
    --

    -- It is possible for the same control to be generated several times.
    -- To avoid exploring them twice, we keep a cache of explored schedules.
    cacheIO :: IO (IORef (Set ScheduleControl))
    cacheIO = newIORef $
              -- we use opts here just to be sure the reference cannot be
              -- lifted out of exploreSimTrace
              if explorationScheduleBound opts >=0
                then Set.empty
                else error "exploreSimTrace: negative schedule bound"

    cachedIO :: ScheduleControl -> IO Bool
    cachedIO m = atomicModifyIORef' cache $ \set ->
      (Set.insert m set, Set.member m set)

    cacheSizeIO :: () -> IO Int
    cacheSizeIO () = Set.size <$> readIORef cache


-- | A specialised version of `controlSimTrace'.
--
-- An internal function.
--
replaySimTrace :: forall a test. (Testable test)
               => ExplorationOptions
               -- ^ race exploration options
               -> (forall s. IOSim s a)
               -> ScheduleControl
               -- ^ a schedule control to reproduce
               -> (SimTrace a -> test)
               -- ^ a callback which receives the simulation trace. The trace
               -- will not contain any race events
               -> Property
replaySimTrace opts mainAction control k =
  let (_,trace) = detachTraceRaces $
                  controlSimTrace (explorationStepTimelimit opts) control mainAction
  in property (k trace)

-- | Run a simulation using a given schedule.  This is useful to reproduce
-- failing cases without exploring the races.
--
controlSimTrace :: forall a.
                   Maybe Int
                -- ^ limit on the computation time allowed per scheduling step, for
                -- catching infinite loops etc.
                -> ScheduleControl
                -- ^ a schedule to replay
                --
                -- /note/: must be either `ControlDefault` or `ControlAwait`.
                -> (forall s. IOSim s a)
                -- ^ a simulation to run
                -> SimTrace a
controlSimTrace limit control mainAction =
    runST (controlSimTraceST limit control mainAction)

raceReversals :: ScheduleControl -> Int
raceReversals ControlDefault      = 0
raceReversals (ControlAwait mods) = length mods
raceReversals ControlFollow{}     = error "Impossible: raceReversals ControlFollow{}"

-- compareTraces is given (maybe) a passing trace and a failing trace,
-- and identifies the point at which they diverge, where it inserts a
-- "sleep" event for the thread that is delayed in the failing case,
-- and a "wake" event before its next action. It also returns the
-- identity and time of the sleeping thread. Since we expect the trace
-- to be consumed lazily (and perhaps only partially), and since the
-- sleeping thread is not of interest unless the trace is consumed
-- this far, then we collect its identity only if it is reached using
-- unsafePerformIO.

compareTraces :: Maybe (SimTrace a1)
              -> SimTrace a2
              -> (Maybe ((Time, ThreadId, Maybe ThreadLabel),
                         Set.Set (ThreadId, Maybe ThreadLabel)),
                  SimTrace a2)
compareTraces Nothing trace = (Nothing, trace)
compareTraces (Just passing) trace = unsafePerformIO $ do
  sleeper <- newIORef Nothing
  return (unsafePerformIO $ readIORef sleeper,
          go sleeper passing trace)
  where go sleeper (SimPORTrace tpass tidpass _ _ _ pass')
                   (SimPORTrace tfail tidfail tstepfail tlfail evfail fail')
          | (tpass,tidpass) == (tfail,tidfail) =
              SimPORTrace tfail tidfail tstepfail tlfail evfail
                $ go sleeper pass' fail'
        go sleeper (SimPORTrace tpass tidpass tsteppass tlpass _ _) fail =
          unsafePerformIO $ do
            writeIORef sleeper $ Just ((tpass, tidpass, tlpass),Set.empty)
            return $ SimPORTrace tpass tidpass tsteppass tlpass EventThreadSleep
                   $ wakeup sleeper tidpass fail
        go _ SimTrace {} _ = error "compareTraces: invariant violation"
        go _ _ SimTrace {} = error "compareTraces: invariant violation"
        go _ _ fail = fail

        wakeup sleeper tidpass
               fail@(SimPORTrace tfail tidfail tstepfail tlfail evfail fail')
          | tidpass == tidfail =
              SimPORTrace tfail tidfail tstepfail tlfail EventThreadWake fail
          | otherwise = unsafePerformIO $ do
              Just (slp,racing) <- readIORef sleeper
              writeIORef sleeper $ Just (slp,Set.insert (tidfail,tlfail) racing)
              return $ SimPORTrace tfail tidfail tstepfail tlfail evfail
                     $ wakeup sleeper tidpass fail'
        wakeup _ _ SimTrace {} = error "compareTraces: invariant violation"
        wakeup _ _ fail = fail
