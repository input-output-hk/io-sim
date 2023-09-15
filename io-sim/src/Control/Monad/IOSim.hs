{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import           Data.Bifunctor (first)
import           Data.Dynamic (fromDynamic)
import           Data.Functor (void)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           Data.List.Trace (Trace (..))
import qualified Data.List.Trace as Trace

import           Control.Exception (throw)

import           Control.Monad.ST.Lazy
import           Data.STRef.Lazy

import           Control.Monad.Class.MonadThrow as MonadThrow

import           Control.Monad.IOSim.Internal (runSimTraceST)
import           Control.Monad.IOSim.Types
import qualified Control.Monad.IOSimPOR.Internal as IOSimPOR (controlSimTraceST)
import           Control.Monad.IOSimPOR.QuickCheckUtils

import           Test.QuickCheck

import           System.IO.Unsafe
import qualified Debug.Trace as Debug


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

detachTraceRacesST :: forall a s. SimTrace a -> ST s (ST s [ScheduleControl], SimTrace a)
detachTraceRacesST trace0 = do
  races <- newSTRef []
  let readRaces :: ST s [ScheduleControl]
      readRaces = concat . reverse <$> readSTRef races

      saveRaces :: [ScheduleControl] -> ST s ()
      saveRaces rs = modifySTRef races (rs:)

      go :: SimTrace a -> ST s (SimTrace a)
      go (SimTrace a b c d trace)      = SimTrace a b c d <$> go trace
      go (SimPORTrace _ _ _ _ EventRaces {} trace)
                                       = go trace
      go (SimPORTrace a b c d e trace) = SimPORTrace a b c d e <$> go trace
      go (TraceRacesFound rs trace)    = saveRaces rs >> go trace
      go t                             = return t

  trace <- go trace0
  return (readRaces, trace)

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
-- clean-up policy then this helps testing for that.
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
-- `Control.Monad.Class.MonadTimer.NonStandard.MonadTimeout` API.  The usual
-- QuickCheck techniques can help explore different schedules of
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
      case runST (do cacheRef <- createCacheST
                     prop <- explore cacheRef (explorationScheduleBound opts) (explorationBranching opts) ControlDefault Nothing
                     size <- cacheSizeST cacheRef
                     return (prop, size)
                 ) of
        (prop, !size) -> tabulate "Modified schedules explored" [bucket size] prop

    Just control ->
      replaySimTrace opts mainAction control (k Nothing)

  where
    opts = optsf stdExplorationOptions

    explore :: forall s.
               STRef s (Set ScheduleControl)
            -> Int -- schedule bound
            -> Int -- branching factor
            -> ScheduleControl -> Maybe (SimTrace a) -> ST s Property
    explore cacheRef n m control passingTrace = do
      traceWithRaces <-  IOSimPOR.controlSimTraceST (explorationStepTimelimit opts) control mainAction
      (readRaces, trace0) <- detachTraceRacesST traceWithRaces
      (readSleeperST, trace) <- compareTracesST passingTrace trace0
      conjoinNoCatchST
        [ do sleeper <- readSleeperST
             () <- traceDebugLog (explorationDebugLevel opts) traceWithRaces
             return $ counterexample ("Schedule control: " ++ show control)
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
        , do let limit = (n+m-1) `div` m
              -- To ensure the set of schedules explored is deterministic, we
              -- filter out cached ones *after* selecting the children of this
              -- node.
             races <- catMaybes
                  <$> (readRaces >>= traverse (cachedST cacheRef) . take limit)
             () <- traceDebugLog (explorationDebugLevel opts) traceWithRaces
             let branching = length races
             -- tabulate "Races explored" (map show races) $
             tabulate "Branching factor" [bucket branching]
                .  tabulate "Race reversals per schedule" [bucket (raceReversals control)]
               <$> conjoinParST
                     [ explore cacheRef n' ((m-1) `max` 1) r (Just trace0)
                     | (r,n') <- zip races (divide (n-branching) branching) ]
        ]

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
      ppIOSimThreadId tid ++ (case lab of Nothing -> ""
                                          Just l  -> " ("++l++")")

    -- insert a schedule into the cache
    cachedST :: STRef s (Set ScheduleControl) -> ScheduleControl -> ST s (Maybe ScheduleControl)
    cachedST cacheRef a = do
      set <- readSTRef cacheRef
      writeSTRef cacheRef (Set.insert a set)
      return $ if Set.member a set
               then Nothing
               else Just a

    --
    -- Caching in ST monad
    --

    -- It is possible for the same control to be generated several times.
    -- To avoid exploring them twice, we keep a cache of explored schedules.
    createCacheST :: ST s (STRef s (Set ScheduleControl))
    createCacheST = newSTRef $
              -- we use opts here just to be sure the reference cannot be
              -- lifted out of exploreSimTrace
              if explorationScheduleBound opts >=0
                then Set.empty
                else error "exploreSimTrace: negative schedule bound"

    cacheSizeST :: STRef s (Set ScheduleControl) -> ST s Int
    cacheSizeST = fmap Set.size . readSTRef


-- |  Trace `SimTrace` to `stderr`.
--
traceDebugLog :: Int -> SimTrace a -> ST s ()
traceDebugLog logLevel _trace | logLevel <= 0 = pure ()
traceDebugLog 1 trace = Debug.traceM $ "Simulation trace with discovered schedules:\n"
                                    ++ Trace.ppTrace show (ppSimEvent 0 0 0) (ignoreRaces $ void `first` trace)
traceDebugLog _ trace = Debug.traceM $ "Simulation trace with discovered schedules:\n"
                                    ++ Trace.ppTrace show (ppSimEvent 0 0 0) (void `first` trace)



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
  let trace = runST $ do
        (_readRaces, trace) <- IOSimPOR.controlSimTraceST (explorationStepTimelimit opts) control mainAction
                           >>= detachTraceRacesST
        return (ignoreRaces trace)
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
controlSimTrace limit control main =
    runST (controlSimTraceST limit control main)

controlSimTraceST :: Maybe Int -> ScheduleControl -> IOSim s a -> ST s (SimTrace a)
controlSimTraceST limit control main =
  ignoreRaces <$> IOSimPOR.controlSimTraceST limit control main


--
-- Utils
--

ignoreRaces :: SimTrace a -> SimTrace a
ignoreRaces = Trace.filter (\a -> case a of
                                    SimPOREvent { seType = EventRaces {} } -> False
                                    _ -> True)

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

-- TODO: return StepId
compareTracesST :: forall a b s.
                   Maybe (SimTrace a) -- ^ passing
                -> SimTrace b         -- ^ failing
                -> ST s ( ST s (Maybe ( (Time, ThreadId, Maybe ThreadLabel)
                                      , Set.Set (ThreadId, Maybe ThreadLabel)
                                      ))
                        , SimTrace b
                        )
compareTracesST Nothing trace = return (return Nothing, trace)
compareTracesST (Just passing) trace = do
  sleeper <- newSTRef Nothing
  trace' <- go sleeper passing trace
  return ( readSTRef sleeper
         , trace'
         )
  where
        go :: STRef s (Maybe ( (Time, ThreadId, Maybe ThreadLabel)
                             , Set.Set (ThreadId, Maybe ThreadLabel)
                             ))
           -> SimTrace a -- ^ passing
           -> SimTrace b -- ^ failing
           -> ST s (SimTrace b)
        go sleeper (SimPORTrace tpass tidpass _ _ _ pass')
                   (SimPORTrace tfail tidfail tstepfail tlfail evfail fail')
          | (tpass,tidpass) == (tfail,tidfail) =
              SimPORTrace tfail tidfail tstepfail tlfail evfail
                <$> go sleeper pass' fail'
        go sleeper (SimPORTrace tpass tidpass tsteppass tlpass _ _) fail = do
          writeSTRef sleeper $ Just ((tpass, tidpass, tlpass),Set.empty)
          SimPORTrace tpass tidpass tsteppass tlpass EventThreadSleep
            <$> wakeup sleeper tidpass fail
        go _ SimTrace {} _ = error "compareTracesST: invariant violation"
        go _ _ SimTrace {} = error "compareTracesST: invariant violation"
        go _ _ fail = return fail

        wakeup :: STRef s (Maybe ( (Time, ThreadId, Maybe ThreadLabel)
                                 , Set.Set (ThreadId, Maybe ThreadLabel)
                                 ))
               -> ThreadId
               -> SimTrace b
               -> ST s (SimTrace b)
        wakeup sleeper tidpass
               fail@(SimPORTrace tfail tidfail tstepfail tlfail evfail fail')
          | tidpass == tidfail =
              return $ SimPORTrace tfail tidfail tstepfail tlfail EventThreadWake fail
          | otherwise = do
              ms <- readSTRef sleeper
              case ms of
                Just (slp, racing) -> do
                  writeSTRef sleeper $ Just (slp,Set.insert (tidfail,tlfail) racing)
                  SimPORTrace tfail tidfail tstepfail tlfail evfail
                    <$> wakeup sleeper tidpass fail'
                Nothing -> error "compareTraceST: invariant violation"
        wakeup _ _ SimTrace {} = error "compareTracesST: invariant violation"
        wakeup _ _ fail = return fail
