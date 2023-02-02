{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Control.Monad.IOSimPOR (tests) where

import           Data.Fixed (Micro)
import           Data.Functor (($>))
import           Data.IORef
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

import           System.Exit
import           System.IO.Error (ioeGetErrorString, isUserError)
import           System.IO.Unsafe

import           Control.Exception (ArithException (..))
import           Control.Monad
import           Control.Monad.Fix
import           Control.Parallel

import           Control.Monad.Class.MonadFork
import           Control.Concurrent.Class.MonadSTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTest
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           GHC.Generics

import           Test.Control.Monad.Utils
import           Test.Control.Monad.STM

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup "IO simulator POR"
  [ testProperty "propSimulates"    propSimulates
  , testProperty "propExploration"  propExploration
  -- , testProperty "propPermutations" propPermutations
  , testGroup "IO simulator properties"
    [ testProperty "read/write graph (IOSim)" (withMaxSuccess 1000 prop_stm_graph_sim)
    , testProperty "timers (IOSim)"           (withMaxSuccess 1000 prop_timers_ST)
    , testProperty "timeout (IOSim): no deadlock"
                                              prop_timeout_no_deadlock_Sim
    , testProperty "threadId order (IOSim)"   (withMaxSuccess 1000 prop_threadId_order_order_Sim)
    , testProperty "forkIO order (IOSim)"     (withMaxSuccess 1000 prop_fork_order_ST)
    , testGroup "throw/catch unit tests"
      [ testProperty "0" unit_catch_0
      , testProperty "1" unit_catch_1
      , testProperty "2" unit_catch_2
      , testProperty "3" unit_catch_3
      , testProperty "4" unit_catch_4
      , testProperty "5" unit_catch_5
      , testProperty "6" unit_catch_6
      ]
    , testGroup "masking state"
      [ testProperty "set (IOSim)"
      $ forall_masking_states unit_set_masking_state_ST

      , testProperty "unmask (IOSim)"
      $ forall_masking_states $ \ms  ->
        forall_masking_states $ \ms' -> unit_unmask_ST ms ms'

      , testProperty "fork (IOSim)"
      $ forall_masking_states unit_fork_masking_state_ST

      , testProperty "fork unmask (IOSim)"
      $ forall_masking_states $ \ms  ->
        forall_masking_states $ \ms' -> unit_fork_unmask_ST ms ms'

      , testProperty "catch (IOSim)"
      $ forall_masking_states unit_catch_throwIO_masking_state_ST

      , testProperty "catch: throwTo (IOSim)"
      $ forall_masking_states unit_catch_throwTo_masking_state_ST

      , testProperty "catch: throwTo async (IOSim)"
      $ forall_masking_states unit_catch_throwTo_masking_state_async_ST

      , testProperty "catch: throwTo async blocking (IOSim)"
      $ forall_masking_states unit_catch_throwTo_masking_state_async_mayblock_ST
      ]
    , testProperty "evaluate unit test" unit_evaluate_0
    , testGroup "forkIO unit tests"
      [ testProperty "1" unit_fork_1
      ]
    , testGroup "async exception unit tests"
      [ testProperty "1"  unit_async_1
      , testProperty "3"  unit_async_3
      , testProperty "4"  unit_async_4
      , testProperty "5"  unit_async_5
      , testProperty "6"  unit_async_6
      , testProperty "7"  unit_async_7
      , testProperty "8"  unit_async_8
      , testProperty "9"  unit_async_9
      ]
    , testGroup "STM reference semantics"
      [ testProperty "Reference vs Sim"   prop_stm_referenceSim
      ]
    , testGroup "MonadFix instance"
      [ testProperty "purity"     prop_mfix_purity
      , testProperty "purity2"    prop_mfix_purity_2
      , testProperty "tightening" prop_mfix_left_shrinking
      , testProperty "lazy"       prop_mfix_lazy
      ]
    ]
  ]

data Step =
    WhenSet Int Int
  | ThrowTo Int
  | Delay Int
  | Timeout TimeoutStep
  | CheckStatus Int
  deriving (Eq, Ord, Show)

data TimeoutStep =
    NewTimeout    Int
  | UpdateTimeout Int
  | CancelTimeout
  | AwaitTimeout
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Step where
  arbitrary = frequency [(5,do m <- choose (1,20)
                               n <- choose (0,m)
                               return $ WhenSet m n),
                         (1,do NonNegative i <- arbitrary
                               return $ ThrowTo i),
                         (1,do Positive i <- arbitrary
                               return $ Delay i),
                         (1,do NonNegative i <- arbitrary
                               return $ CheckStatus i),
                         (1,Timeout <$> arbitrary)]

  shrink (WhenSet m n) = map (WhenSet m) (shrink n) ++
                         map (`WhenSet` n) (filter (>=n) (shrink m))
  shrink (ThrowTo i) = map ThrowTo (shrink i)
  shrink (CheckStatus i) = map CheckStatus (shrink i)
  shrink (Delay i)   = map Delay (shrink i)
  shrink (Timeout t) = map Timeout (shrink t)

instance Arbitrary TimeoutStep where
  arbitrary = do Positive i <- arbitrary
                 frequency $ map (fmap return) $
                   [(3,NewTimeout i),
                    (1,UpdateTimeout i),
                    (1,CancelTimeout),
                    (3,AwaitTimeout)]

  shrink = genericShrink


newtype Task = Task [Step]
  deriving (Eq, Ord, Show)

instance Arbitrary Task where
  arbitrary = do
    steps <- arbitrary
    return . Task $ normalize steps
  shrink (Task steps) =
    (Task <$> compressSteps steps) ++
    (Task . normalize <$> shrink steps)

normalize :: [Step] -> [Step]
normalize steps = plug steps wsSteps 1000000
  where wsSteps = reverse $ sort [s | s@(WhenSet _ _) <- steps]
        plug []              []               _ = []
        plug (WhenSet _ _:s) (WhenSet a b:ws) m = WhenSet (min a m) (min b m):plug s ws (min b m)
        plug (step:s)        ws               m = step:plug s ws m
        plug _               _                _ = error "plug: impossible"

compressSteps :: [Step] -> [[Step]]
compressSteps (WhenSet a b:WhenSet c d:steps) =
  [WhenSet a d:steps] ++ ((WhenSet a b:) <$> compressSteps (WhenSet c d:steps))
compressSteps (s:steps) = (s:) <$> compressSteps steps
compressSteps [] = []

newtype Tasks = Tasks [Task]
  deriving Show

instance Arbitrary Tasks where
  arbitrary = Tasks . fixSymbolicThreadIds <$> scale (min 20) arbitrary
  shrink (Tasks ts) = Tasks . fixSymbolicThreadIds <$>
         removeTask ts ++
         shrink ts ++
         shrinkDelays ts ++
         advanceThrowTo ts ++
         sortTasks ts

fixSymbolicThreadIds :: [Task] -> [Task]
fixSymbolicThreadIds tasks = mapSymThreadIds (`mod` length tasks) tasks

shrinkDelays :: [Task] -> [[Task]]
shrinkDelays tasks
  | null times = []
  | otherwise  = [map (Task . removeTime d) [steps | Task steps <- tasks]
                 | d <- times]
  where times = foldr union [] [scanl1 (+) [d | Delay d <- t] | Task t <- tasks]
        removeTime 0 steps = steps
        removeTime _ []    = []
        removeTime d (Delay d':steps)
          | d==d' = steps
          | d< d' = Delay (d'-d):steps
          | d> d' = removeTime (d-d') steps
        removeTime d (s:steps) =
          s:removeTime d steps

removeTask :: [Task] -> [[Task]]
removeTask tasks =
  [ mapThrowTos (fixup i) . map (dontThrowTo i) $ take i tasks++drop (i+1) tasks
  | i <- [0..length tasks-1]]
  where fixup i j | j>i       = j-1
                  | otherwise = j
        dontThrowTo i (Task steps) = Task (filter (/=ThrowTo i) steps)

advanceThrowTo :: [Task] -> [[Task]]
advanceThrowTo [] = []
advanceThrowTo (Task steps:ts) =
  ((:ts) . Task <$> advance steps) ++
  ((Task steps:) <$> advanceThrowTo ts)
  where advance (WhenSet a b:ThrowTo i:steppes) =
          [ThrowTo i:WhenSet a b:steppes] ++ (([WhenSet a b,ThrowTo i]++) <$> advance steppes)
        advance (s:steppes) = (s:) <$> advance steppes
        advance []          = []

mapSymThreadIds :: (Int -> Int) -> [Task] -> [Task]
mapSymThreadIds f tasks = map mapTask tasks
  where mapTask (Task steps) = Task (map mapStep steps)
        mapStep (ThrowTo i) = ThrowTo (f i)
        mapStep (CheckStatus i) = CheckStatus (f i)
        mapStep s           = s

mapThrowTos :: (Int -> Int) -> [Task] -> [Task]
mapThrowTos f tasks = map mapTask tasks
  where mapTask (Task steps) = Task (map mapStep steps)
        mapStep (ThrowTo i) = ThrowTo (f i)
        mapStep s           = s

sortTasks :: Ord a => [a] -> [[a]]
sortTasks (x:y:xs) | x>y = [y:x:xs] ++ ((x:) <$> sortTasks (y:xs))
sortTasks (x:xs)         = (x:) <$> sortTasks xs
sortTasks []             = []

interpret :: forall s. TVar (IOSim s) Int -> TVar (IOSim s) [ThreadId (IOSim s)] -> Task -> IOSim s (ThreadId (IOSim s))
interpret r t (Task steps) = forkIO $ do
    context <- atomically $ do
      ts <- readTVar t
      when (null ts) retry
      timer <- newTVar Nothing
      return (ts,timer)
    mapM_ (interpretStep context) steps
  where interpretStep _ (WhenSet m n) = atomically $ do
          a <- readTVar r
          when (a/=m) retry
          writeTVar r n
        interpretStep (ts,_) (ThrowTo i) = throwTo (ts !! i) (ExitFailure 0)
        interpretStep (ts,_) (CheckStatus i) = void $ threadStatus (ts !! i)
        interpretStep _      (Delay i)   = threadDelay (fromIntegral i)
        interpretStep (_,timer) (Timeout tstep) = do
          timerVal <- atomically $ readTVar timer
          case (timerVal,tstep) of
            (_,NewTimeout n)            -> do tout <- newTimeout (fromIntegral n)
                                              atomically $ writeTVar timer (Just tout)
            (Just tout,UpdateTimeout n) -> updateTimeout tout (fromIntegral n)
            (Just tout,CancelTimeout)   -> cancelTimeout tout
            (Just tout,AwaitTimeout)    -> atomically $ awaitTimeout tout >> return ()
            (Nothing,_)                 -> return ()

runTasks :: [Task] -> IOSim s (Int,Int)
runTasks tasks = do
  let m = maximum [maxTaskValue t | Task t <- tasks]
  r  <- atomically $ newTVar m
  t  <- atomically $ newTVar []
  exploreRaces
  ts <- mapM (interpret r t) tasks
  atomically $ writeTVar t ts
  threadDelay 1000000000  -- allow the SUT threads to run
  a  <- atomically $ readTVar r
  return (m,a)

maxTaskValue :: [Step] -> Int
maxTaskValue (WhenSet m _:_) = m
maxTaskValue (_:t)           = maxTaskValue t
maxTaskValue []              = 0

propSimulates :: Tasks -> Property
propSimulates (Tasks tasks) =
  any (not . null . (\(Task steps)->steps)) tasks ==>
    let Right (m,a) = runSim (runTasks tasks) in
    m>=a

propExploration :: Tasks -> Property
propExploration (Tasks tasks) =
  -- Debug.trace ("\nTasks:\n"++ show tasks) $
  any (not . null . (\(Task steps)->steps)) tasks ==>
    traceNoDuplicates $ \addTrace ->
    --traceCounter $ \addTrace ->
    exploreSimTrace id (runTasks tasks) $ \_ trace ->
    --Debug.trace (("\nTrace:\n"++) . splitTrace . noExceptions $ show trace) $
    addTrace trace $
    counterexample (splitTrace . noExceptions $ show trace) $
    case traceResult False trace of
      Right (m,a) -> property $ m>=a
      Left e      -> counterexample (show e) False

-- Testing propPermutations n should collect every permutation of [1..n] once only.
-- Test manually, and supply a small value of n.
propPermutations :: Int -> Property
propPermutations n =
  traceNoDuplicates $ \addTrace ->
  exploreSimTrace (withScheduleBound 10000) (doit n) $ \_ trace ->
    addTrace trace $
    let Right result = traceResult False trace in
    tabulate "Result" [noExceptions $ show $ result] $
      True

doit :: Int -> IOSim s [Int]
doit n = do
          r <- atomically $ newTVar []
          exploreRaces
          mapM_ (\i -> forkIO $ atomically $ modifyTVar r (++[i])) [1..n]
          threadDelay 1
          atomically $ readTVar r

ordered :: Ord a => [a] -> Bool
ordered xs = and (zipWith (<) xs (drop 1 xs))

noExceptions :: [Char] -> [Char]
noExceptions xs = unsafePerformIO $ try (evaluate xs) >>= \case
  Right []     -> return []
  Right (x:ys) -> return (x:noExceptions ys)
  Left e       -> return ("\n"++show (e :: SomeException))

splitTrace :: [Char] -> [Char]
splitTrace [] = []
splitTrace (x:xs) | begins "(Trace" = "\n(" ++ splitTrace xs
                  | otherwise       = x:splitTrace xs
  where begins s = take (length s) (x:xs) == s

traceCounter :: (Testable prop1, Show a1) => ((a1 -> a2 -> a2) -> prop1) -> Property
traceCounter k = r `pseq` (k addTrace .&&.
                           tabulate "Trace repetitions" (map show $ traceCounts ()) True)
  where
    r = unsafePerformIO $ newIORef (Map.empty :: Map String Int)
    addTrace t x = unsafePerformIO $ do
      atomicModifyIORef r (\m->(Map.insertWith (+) (show t) 1 m,()))
      return x
    traceCounts () = unsafePerformIO $ Map.elems <$> readIORef r

traceNoDuplicates :: (Testable prop1, Show a1) => ((a1 -> a2 -> a2) -> prop1) -> Property
traceNoDuplicates k = r `pseq` (k addTrace .&&. maximum (traceCounts ()) == 1)
  where
    r = unsafePerformIO $ newIORef (Map.empty :: Map String Int)
    addTrace t x = unsafePerformIO $ do
      atomicModifyIORef r (\m->(Map.insertWith (+) (show t) 1 m,()))
      return x
    traceCounts () = unsafePerformIO $ Map.elems <$> readIORef r

--
-- IOSim reused properties
--


--
-- Read/Write graph
--

prop_stm_graph_sim :: TestThreadGraph -> Property
prop_stm_graph_sim g =
  traceNoDuplicates $ \addTrace ->
    exploreSimTrace id (prop_stm_graph g) $ \_ trace ->
      addTrace trace $
      counterexample (splitTrace . noExceptions $ show trace) $
      case traceResult False trace of
        Right () -> property True
        Left e   -> counterexample (show e) False
      -- TODO: Note that we do not use runSimStrictShutdown here to check
      -- that all other threads finished, but perhaps we should and structure
      -- the graph tests so that's the case.

prop_timers_ST :: TestMicro -> Property
prop_timers_ST (TestMicro xs) =
  let ds = map (realToFrac :: Micro -> DiffTime) xs
   in exploreSimTrace id (test_timers ds) $ \_ trace ->
        case traceResult False trace of
          Right a -> a
          Left e  -> counterexample (show e) False

--
-- Forking
--

prop_fork_order_ST :: Positive Int -> Property
prop_fork_order_ST n =
   exploreSimTrace id (test_fork_order n) $ \_ trace ->
     case traceResult False trace of
       Right a -> a
       Left e  -> counterexample (show e) False

prop_threadId_order_order_Sim :: Positive Int -> Property
prop_threadId_order_order_Sim n =
   exploreSimTrace id (test_threadId_order n) $ \_ trace ->
     case traceResult False trace of
       Right a -> a
       Left e  -> counterexample (show e) False

--
-- MonadFix properties
--

-- | Purity demands that @mfix (return . f) = return (fix f)@.
--
prop_mfix_purity :: Positive Int -> Property
prop_mfix_purity (Positive n) =
   exploreSimTrace id (mfix (return . factorial)) $ \_ trace ->
     case traceResult False trace of
       Right f -> f n === fix factorial n
       Left e  -> counterexample (show e) False
  where
    factorial :: (Int -> Int) -> Int -> Int
    factorial = \rec_ k -> if k <= 1 then 1 else k * rec_ (k - 1)

prop_mfix_purity_2 :: [Positive Int] -> Property
prop_mfix_purity_2 as =
   -- note: both 'IOSim' expressions are equivalent using 'Monad' and
   -- 'Applicative' laws only.
   exploreSimTrace id (join $ mfix (return . recDelay)
                             <*> return as') (\_ trace ->
     case traceResult False trace of
       Right a -> a === expected
       Left e  -> counterexample (show e) False)
   .&&.
   exploreSimTrace id (mfix (return . recDelay) >>= ($ as')) (\_ trace ->
     case traceResult False trace of
       Right a -> a === expected
       Left e  -> counterexample (show e) False)
  where
    as' :: [Int]
    as' = getPositive `map` as

    -- recursive sum using 'threadDelay'
    recDelay :: ( MonadMonotonicTime m
                , MonadDelay m
                )
             => ([Int] -> m Time)
             ->  [Int] -> m Time
    recDelay = \rec_ bs ->
                 case bs of
                  []        -> getMonotonicTime
                  (b : bs') -> threadDelay (realToFrac b)
                            >> rec_ bs'

    expected :: Time
    expected = foldl' (flip addTime)
                      (Time 0)
                      (realToFrac `map` as')

prop_mfix_left_shrinking
    :: Int
    -> NonNegative Int
    -> Positive Int
    -> Property
prop_mfix_left_shrinking n (NonNegative d) (Positive i) =
   let mn :: IOSim s Int
       mn = do say ""
               threadDelay (realToFrac d)
               return n
    in exploreSimTrace id (mfix (\rec_ -> mn >>= \a ->
                                    threadDelay (realToFrac d) $> a : rec_))
                          (\_ trace1 ->
       exploreSimTrace id (mn >>= \a ->
                              mfix (\rec_ ->
                                       threadDelay (realToFrac d) $> a : rec_))
                          (\_ trace2 ->
         case (traceResult False trace1, traceResult False trace2) of
           (Right a , Right b)  -> take i a === take i b
           (Left  e , Right _)  -> counterexample (show e) False
           (Right _ , Left  e)  -> counterexample (show e) False
           (Left  e , Left  e') -> counterexample (show e ++ " " ++ show e') False))


-- | 'Example 8.2.1' in 'Value Recursion in Monadic Computations'
-- <https://leventerkok.github.io/papers/erkok-thesis.pdf>
--
prop_mfix_lazy :: NonEmptyList Char
               -> Property
prop_mfix_lazy (NonEmpty env) =
   exploreSimTrace id (withEnv (mfix . replicateHeadM)) (\_ trace ->
     case traceResult False trace of
       Right a -> take samples a === replicate samples (head env)
       Left e  -> counterexample (show e) False)
    where
      samples :: Int
      samples = 10

      replicateHeadM ::
                        (

                          MonadFail m,
                          MonadFail (STM m),

                          MonadSTM  m
                        )
                     => m Char
                     -> String -> m String
      replicateHeadM getChar_ as = do
        -- Note: 'getChar' will be executed only once! This follows from 'fixIO`
        -- semantics.
        a <- getChar_
        return (a : as)

      -- construct 'getChar' using the simulated environment
      withEnv :: (

                   MonadFail m,

                   MonadSTM  m
                 )
              => (m Char -> m a) -> m a
      withEnv k = do
        v <- newTVarIO env
        let getChar_ =
              atomically $ do
                as <- readTVar v
                case as of
                  [] -> error "withEnv: runtime error"
                  (a : as') -> writeTVar v as'
                            $> a
        k getChar_

--
-- Syncronous exceptions
--

unit_catch_0, unit_catch_1, unit_catch_2, unit_catch_3, unit_catch_4,
  unit_catch_5, unit_catch_6, unit_fork_1
  :: Property


--   exploreSimTrace id (withEnv (mfix . replicateHeadM)) (\_ trace ->
--     case traceResult False trace of
--       Right a -> take samples a === replicate samples (head env)
--       Left e  -> counterexample (show e) False)

-- unhandled top level exception
unit_catch_0 =
  exploreSimTrace id example $ \_ trace ->
    counterexample (intercalate "\n" $ map show $ traceEvents trace) $
    counterexample (show $ selectTraceSay trace) $
    selectTraceSay trace === ["before"]
    .&&.
    case traceResult True trace of
      Left (FailureException e) -> property ((Just DivideByZero ==) $ fromException e)
      _                         -> property False
 where
  example :: IOSim s ()
  example = do
    say "before"
    _ <- throwIO DivideByZero
    say "after"

-- normal execution of a catch frame
unit_catch_1 =
  exploreSimTrace id (do catch (say "inner")
                               (\(_e :: IOError) -> say "handler")
                         say "after")
                         $ \_ trace ->
    selectTraceSay trace === ["inner", "after"]

-- catching an exception thrown in a catch frame
unit_catch_2 =
  exploreSimTrace id
                  (do catch (do say "inner1"
                                _ <- throwIO DivideByZero
                                say "inner2")
                            (\(_e :: ArithException) -> say "handler")
                      say "after"
                   ) $ \_ trace ->
    selectTraceSay trace === ["inner1", "handler", "after"]

-- not catching an exception of the wrong type
unit_catch_3 =
  exploreSimTrace id
                  (do catch (do say "inner"
                                throwIO DivideByZero)
                            (\(_e :: IOError) -> say "handler")
                      say "after"
                  ) $ \_ trace ->
    selectTraceSay trace === ["inner"]


-- catching an exception in an outer handler
unit_catch_4 =
  exploreSimTrace id
                  (do catch (catch (do say "inner"
                                       throwIO DivideByZero)
                                   (\(_e :: IOError) -> say "handler1"))
                            (\(_e :: ArithException) -> say "handler2")
                      say "after"
                  ) $ \_ trace ->
    selectTraceSay trace === ["inner", "handler2", "after"]


-- catching an exception in the inner handler
unit_catch_5 =
  exploreSimTrace id
                  (do catch (catch (do say "inner"
                                       throwIO DivideByZero)
                                   (\(_e :: ArithException) -> say "handler1"))
                            (\(_e :: ArithException) -> say "handler2")
                      say "after"
                  ) $ \_ trace ->
     selectTraceSay trace === ["inner", "handler1", "after"]

-- catching an exception in the inner handler, rethrowing and catching in outer
unit_catch_6 =
  exploreSimTrace id
                  (do catch (catch (do say "inner"
                                       throwIO DivideByZero)
                                   (\(e :: ArithException) -> do
                                     say "handler1"
                                     throwIO e))
                            (\(_e :: ArithException) -> say "handler2")
                      say "after"
                   ) $ \_ trace ->
    selectTraceSay trace === ["inner", "handler1", "handler2", "after"]

-- evaluate should catch pure errors
unit_evaluate_0 :: Property
unit_evaluate_0 =
    -- This property also fails if the @error@ is not caught by the sim monad
    -- and instead reaches the QuickCheck driver.
    -- property $ isLeft $ runSim $ evaluate (error "boom" :: ())
  exploreSimTrace id (evaluate (error "boom" :: ())) $ \_ trace ->
    case traceResult False trace of
      Right _ -> counterexample "didn't fail" False
      Left _  -> property True


-- Try works and we can pass exceptions back from threads.
-- And terminating with an exception is reported properly.
unit_fork_1 =
  exploreSimTrace id example $ \_ trace ->
    selectTraceSay trace === ["parent", "user error (oh noes!)"]
    .&&. case traceResult True trace of
          Left (FailureException e)
            | Just ioe <- fromException e
            , isUserError ioe
            , ioeGetErrorString ioe == "oh noes!" -> property True
          _                                       -> property False
  where
    example :: IOSim s ()
    example = do
      resVar <- newEmptyTMVarIO
      void $ forkIO $ do
        res <- try (fail "oh noes!")
        atomically (putTMVar resVar (res :: Either SomeException ()))
      say "parent"
      Left e <- atomically (takeTMVar resVar)
      say (show e)
      throwIO e


--
-- Asyncronous exceptions
--

unit_async_1, unit_async_2, unit_async_3, unit_async_4,
  unit_async_5, unit_async_6, unit_async_7, unit_async_8,
  unit_async_9
  :: Property


unit_async_1 =
  exploreSimTrace id
                  (do tid <- myThreadId
                      say "before"
                      throwTo tid DivideByZero
                      say "after"
                  ) $ \_ trace ->
    selectTraceSay trace === ["before"]


unit_async_2 =
    runSimTraceSay
      (do tid <- myThreadId
          catch (do say "before"
                    throwTo tid DivideByZero
                    say "never")
                (\(_e :: ArithException) -> say "handler"))
 ===
   ["before", "handler"]


unit_async_3 =
  exploreSimTrace id
                  (do tid <- forkIO $ say "child"
                      threadDelay 1
                      -- child has already terminated when we throw the async exception
                      throwTo tid DivideByZero
                      say "parent done"
                  ) $ \_ trace ->
    selectTraceSay trace === ["child", "parent done"]


unit_async_4 =
  exploreSimTrace id
                  (do tid <- forkIO $ do
                              say "child"
                              catch (atomically retry)
                                    (\(_e :: ArithException) -> say "handler")
                              say "child done"
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done"
                  ) $ \_ trace ->
    selectTraceSay trace === ["child", "handler", "child done", "parent done"]


unit_async_5 =
  exploreSimTrace id
                  (do tid <- forkIO $ mask_ $
                               do
                                 say "child"
                                 threadDelay 1
                                 say "child masked"
                                 -- while masked, do a blocking (interruptible) operation
                                 catch (atomically retry)
                                     (\(_e :: ArithException) -> say "handler")
                                 say "child done"
                      -- parent and child wake up on the runqueue at the same time
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done"
                  ) $ \_ trace ->
    selectTraceSay trace === ["child", "child masked", "handler", "child done", "parent done"]


unit_async_6 =
  exploreSimTrace id
                  (do tid <- forkIO $
                               mask_ $ do
                                 say "child"
                                 threadDelay 1
                                 fail "oh noes!"
                      -- parent and child wake up on the runqueue at the same time
                      threadDelay 1
                      throwTo tid DivideByZero
                      -- throwTo blocks but then unblocks because the child dies
                      say "parent done") $ \_ trace ->
    selectTraceSay trace === ["child", "parent done"]


unit_async_7 =
  exploreSimTrace id
                  (do tid <- forkIO $ do
                               uninterruptibleMask_ $ do
                                 say "child"
                                 threadDelay 1
                                 say "child masked"
                                 -- while masked, do a blocking (interruptible) operation
                                 catch (threadDelay 1)
                                     (\(_e :: ArithException) -> say "handler")
                                 say "child done"
                               say "never"
                      -- parent and child wake up on the runqueue at the same time
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done") $ \_ trace ->
    selectTraceSay trace === ["child", "child masked", "child done", "parent done"]


unit_async_8 =
  exploreSimTrace id
                  (uninterruptibleMask_ $ do
                     tid <- forkIO $ atomically retry
                     throwTo tid DivideByZero) $ \_ trace ->
    case traceResult False trace of
      Left FailureDeadlock {} -> property True
      _                       -> property False


unit_async_9 =
  exploreSimTrace id
                  (do tid <- forkIO $ do
                               uninterruptibleMask_ $ do
                                 say "child"
                                 threadDelay 1
                                 say "child masked"
                                 -- while masked do a blocking operation, but this is
                                 -- an uninterruptible mask so nothing happens
                                 catch (threadDelay 1)
                                     (\(_e :: ArithException) -> say "handler")
                                 say "child done"
                               say "never"
                      threadDelay 1
                      throwTo tid DivideByZero
                      threadDelay 1
                      say "parent done") $ \_ trace ->
    selectTraceSay trace === ["child", "child masked", "child done", "parent done"]


--
-- Tests vs STM operational semantics
--

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the IO simulator's STM implementation.
--
prop_stm_referenceSim :: SomeTerm -> Property
prop_stm_referenceSim t =
  exploreSimTrace id (prop_stm_referenceM t) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

prop_timeout_no_deadlock_Sim :: Property
prop_timeout_no_deadlock_Sim = -- runSimOrThrow prop_timeout_no_deadlockM
  exploreSimTrace id prop_timeout_no_deadlockM $ \_ trace ->
    case traceResult False trace of
      Right a -> property a
      Left e  -> counterexample (show e) False

--
-- MonadMask properties
--

unit_set_masking_state_ST :: MaskingState -> Property
unit_set_masking_state_ST ms =
  exploreSimTrace id (prop_set_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_unmask_ST :: MaskingState -> MaskingState -> Property
unit_unmask_ST ms ms' =
  exploreSimTrace id (prop_unmask ms ms') $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_fork_masking_state_ST :: MaskingState -> Property
unit_fork_masking_state_ST ms =
  exploreSimTrace id (prop_fork_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_fork_unmask_ST :: MaskingState -> MaskingState -> Property
unit_fork_unmask_ST ms ms' =
  exploreSimTrace id (prop_fork_unmask ms ms') $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwIO_masking_state_ST :: MaskingState -> Property
unit_catch_throwIO_masking_state_ST ms =
  exploreSimTrace id (prop_catch_throwIO_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwTo_masking_state_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_ST ms =
  exploreSimTrace id (prop_catch_throwTo_masking_state ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwTo_masking_state_async_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_ST ms =
  exploreSimTrace id (prop_catch_throwTo_masking_state_async ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False

unit_catch_throwTo_masking_state_async_mayblock_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_mayblock_ST ms =
  exploreSimTrace id (prop_catch_throwTo_masking_state_async_mayblock ms) $ \_ trace ->
    case traceResult False trace of
      Right a -> a
      Left e  -> counterexample (show e) False
